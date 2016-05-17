/***************************************************************************/
/*                                                                         */
/*                 Make OCaml native debugging awesome!                    */
/*                                                                         */
/*         Mark Shinwell and Frederic Bour, Jane Street Europe             */
/*                                                                         */
/*  Copyright (c) 2013--2016 Jane Street Group, LLC                        */
/*                                                                         */
/*  Permission is hereby granted, free of charge, to any person obtaining  */
/*  a copy of this software and associated documentation files             */
/*  (the "Software"), to deal in the Software without restriction,         */
/*  including without limitation the rights to use, copy, modify, merge,   */
/*  publish, distribute, sublicense, and/or sell copies of the Software,   */
/*  and to permit persons to whom the Software is furnished to do so,      */
/*  subject to the following conditions:                                   */
/*                                                                         */
/*  The above copyright notice and this permission notice shall be         */
/*  included in all copies or substantial portions of the Software.        */
/*                                                                         */
/*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        */
/*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     */
/*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. */
/*  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY   */
/*  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,   */
/*  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE      */
/*  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                 */
/*                                                                         */
/***************************************************************************/

#define CAML_NAME_SPACE 1

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "defs.h"
#include "frame.h"
#include "infcall.h"
#include "stack.h"
#include "symtab.h"
#include "valprint.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

extern value caml_make_vect (value, value);
/* These are initialized by way of [_initialize_ocaml_language]. */
static int value_printer_max_depth;
static char* search_path = NULL;

int
monda_init (void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup (argv);
  return 1;
}

void
monda_val_print (struct type* type, const gdb_byte* valaddr,
                 int embedded_offset, CORE_ADDR address,
                 struct ui_file* stream, int recurse, const struct value* val,
                 const struct value_print_options* options, int depth)
{
  CAMLparam0();
  CAMLlocal4(v_type, v_stream, v_value, v_search_path);
  CAMLlocalN(args, 6);
  static value* callback = NULL;

  if (callback == NULL) {
    callback = caml_named_value("From_gdb_ocaml.print_value");
    assert (callback != NULL);
  }

  v_value = caml_copy_nativeint(*(intnat*) valaddr);
/*fprintf(stderr, "monda_val_print.  valaddr=%p *valaddr=%p\n",
  (void*) valaddr, *(void**) valaddr);*/

  if (TYPE_NAME(type) == NULL) {
    goto print_as_c;
  }

  v_type = caml_copy_string(TYPE_NAME(type));
  v_stream = caml_copy_int64((uint64_t) stream);
  v_search_path = caml_copy_string(search_path ? search_path : "");

  Store_field(args, 0, v_value);
  Store_field(args, 1, v_stream);
  Store_field(args, 2, v_type);
  Store_field(args, 3, Val_bool(options->summary));
  Store_field(args, 4, Val_long(value_printer_max_depth));
  Store_field(args, 5, v_search_path);

  if (caml_callbackN(*callback, 6, args) == Val_false) {
print_as_c:
    c_val_print(type, valaddr, embedded_offset, address, stream, recurse,
      val, options, depth);
  }

  CAMLreturn0;
}

int
monda_parse (const char* expr, int length)
{
  value v_expr;
  static value* cb = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("From_gdb_ocaml.parse");
    assert(cb != NULL);
  }

  v_expr = caml_alloc_string(length);
  memcpy(String_val(v_expr), expr, length);

  if (caml_callback(*cb, v_expr) == Val_true) {
    /* Failure */
    return 1;
  }

  return 0;
}

CORE_ADDR
monda_evaluate (const char* expr, int length)
{
  CAMLparam0();
  CAMLlocal2(v_stream, v_expr);
  value v_result;
  static value* cb = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("From_gdb_ocaml.evaluate");
    assert(cb != NULL);
  }

  v_expr = caml_alloc_string(length);
  memcpy(String_val(v_expr), expr, length);
printf("monda_evaluate: '%s'\n", String_val(v_expr));fflush(stdout);

  v_stream = caml_copy_int64((uint64_t) stderr_fileopen());

  v_result = caml_callback3(*cb, v_expr,
    caml_copy_string(search_path ? search_path : ""),
    v_stream);

  if (v_result == Val_unit /* None */) {
    return (CORE_ADDR) 0;  /* CR mshinwell: suboptimal? */
  }
printf("monda_evaluate is returning %p\n", (void*)Field(v_result, 0));
fflush(stdout);

  return (CORE_ADDR) Field(v_result, 0);
}

char*
monda_demangle (char* mangled, int options)
{
  CAMLparam0();
  CAMLlocal2(caml_res, caml_mangled);
  static value*cb = NULL;
  char* res = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("From_gdb_ocaml.demangle");
    assert(cb != NULL);
  }

  caml_mangled = caml_copy_string(mangled);
  caml_res = caml_callback(*cb, caml_mangled);

  if (Is_block(caml_res)) {
    gdb_assert(Tag_val(caml_res) == 0 && Wosize_val(caml_res) == 1);
    res = strdup(String_val(Field(caml_res, 0)));
  }

  CAMLreturnT (char*, res);
}

void
monda_set_value_printer_max_depth(int new_max_depth)
{
  value_printer_max_depth = new_max_depth;
}

void
monda_set_search_path(char *new_search_path)
{
  if (search_path) {
    free(search_path);
  }
  if (new_search_path) {
    search_path = strdup(new_search_path);
  }
}

/*
value
gdb_ocaml_value_printer_max_depth(value v_unit)
{
  return Val_long(value_printer_max_depth);
}

value
gdb_ocaml_search_path(value v_unit)
{
  return caml_copy_string(search_path);
}
*/
