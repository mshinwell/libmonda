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
  v_type = caml_copy_string(TYPE_NAME(type));
  v_search_path = caml_copy_string(search_path);

  Store_field(args, 0, v_value);
  Store_field(args, 1, v_stream);
  Store_field(args, 2, v_type);
  Store_field(args, 3, Val_bool(options->summary));
  Store_field(args, 4, Val_long(value_printer_max_depth));
  Store_field(args, 5, v_search_path);

  (void) caml_callbackN(*callback, 6, args);

  CAMLreturn0;
}

char*
monda_demangle (char* mangled, int options)
{
  CAMLparam0();
  CAMLlocal2(caml_res, caml_mangled);
  static value *cb = NULL;
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
  search_path = strdup(new_search_path);
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
