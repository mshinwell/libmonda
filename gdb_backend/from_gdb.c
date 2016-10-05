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
static int value_printer_max_string_length;
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
  CAMLlocal1(v_val);
  CAMLlocalN(args, 9);
  static value* callback = NULL;
  int is_synthetic_pointer;

  /* The try/catch is required so we don't leave local roots incorrectly
     registered in the case of an exception.

     We also ensure that any GDB function we call from the OCaml code
     invoked below (via [caml_callbackN]) never throws any exceptions
     across the OCaml -> C boundary.  If it were to, then we would fail to
     run the second part of the [caml_start_program] code, causing global
     variables (e.g. [caml_last_return_address]) to be set incorrectly. */
  TRY {
    if (callback == NULL) {
      callback = caml_named_value("From_gdb_ocaml.print_value");
      assert (callback != NULL);
    }

    v_value = caml_copy_nativeint(*(intnat*) valaddr);

    /* Determine whether the value is actually a construction made up in the
       debugger's address space by virtue of interpreting DW_OP_implicit_pointer.
       The second part of this conditional is really just a sanity check.
    */
    is_synthetic_pointer =
      (value_lval_const(val) == lval_computed
        && value_bits_synthetic_pointer(val, 0, sizeof(CORE_ADDR) * 8));
/*
    fprintf(stderr, "monda_val_print.  SP %d *valaddr=%p v_value=%p  value_lval_const=%d lval_funcs=%p lazy=%d\n",
      is_synthetic_pointer,
      (void*) *(intnat*) valaddr,
      (void*) v_value,
      (int) (value_lval_const(val)),
      value_lval_const(val) == lval_computed ? value_computed_funcs(val) : NULL,
      value_lazy(val));
      */

    /* CR mshinwell: improve this test */
    if ((TYPE_NAME(type) == NULL && !is_synthetic_pointer)
        || (is_synthetic_pointer && TYPE_CODE(type) != TYPE_CODE_PTR)) {
      /*
      fprintf(stderr, "monda_val_print -> c_val_print (1)\n");
      fflush(stderr);
      */
      c_val_print(type, valaddr, embedded_offset, address, stream, recurse,
                  val, options, depth);
    }
    else {
      v_type = caml_copy_string(TYPE_NAME(type) == NULL ? "" : TYPE_NAME(type));
      v_stream = caml_copy_int64((uint64_t) stream);
      v_search_path = caml_copy_string(search_path ? search_path : "");
      v_val = caml_copy_nativeint((intnat) val);

      Store_field(args, 0, Val_bool(is_synthetic_pointer));
      Store_field(args, 1, v_value);
      Store_field(args, 2, v_val);
      Store_field(args, 3, v_stream);
      Store_field(args, 4, v_type);
      Store_field(args, 5, Val_bool(options->summary));
      Store_field(args, 6, Val_long(value_printer_max_depth));
      Store_field(args, 7, Val_long(value_printer_max_string_length));
      Store_field(args, 8, v_search_path);

      /*
      fprintf(stderr, "monda_val_print -> OCaml printer.  Type '%s'\n", TYPE_NAME(type));
      fflush(stderr); */

      if (caml_callbackN(*callback, 8, args) == Val_false) {
        /*
        fprintf(stderr, "monda_val_print -> c_val_print (2)\n");
        fflush(stderr);
        */
        c_val_print(type, valaddr, embedded_offset, address, stream, recurse,
          val, options, depth);
      }
    }
  }
  CATCH (exn, RETURN_MASK_ALL) {
    CAMLdrop;
    throw_exception(exn);
  }
  END_CATCH

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
monda_evaluate (const char* expr, int length, char** type_name_out)
{
  CAMLparam0();
  CAMLlocal2(v_stream, v_expr);
  value v_result;
  static value* cb = NULL;
  int failed = 0;
/*
printf("monda_evaluate '%s'\n", expr);fflush(stdout);
*/

  /* Note for the future: it seems that installing an exception handler in
     this function causes a segfault when trying to run a NULL gdb cleanup
     function. */

  if (cb == NULL) {
    cb = caml_named_value ("From_gdb_ocaml.evaluate");
    assert(cb != NULL);
  }

  v_expr = caml_alloc_string(length);
  memcpy(String_val(v_expr), expr, length);

  /* We assume [stderr_fileopen] doesn't raise any exceptions. */
  v_stream = caml_copy_int64((uint64_t) stderr_fileopen());

  v_result = caml_callback3(*cb, v_expr,
    caml_copy_string(search_path ? search_path : ""),
    v_stream);

  if (v_result == Val_unit /* Failure */) {
    CAMLreturn((CORE_ADDR) 0);  /* CR mshinwell: suboptimal? */
  }
  /*
  printf("monda_evaluate is returning %p, type name '%s'\n",
    (void*)Nativeint_val(Field(v_result, 0)),
    String_val(Field(v_result, 1)));
  fflush(stdout);
  */

  *type_name_out = xstrdup(String_val(Field(v_result, 1)));

  CAMLreturn(
    failed ? (CORE_ADDR) 0 : (CORE_ADDR) Nativeint_val(Field(v_result, 0)));
}

void
monda_set_value_printer_max_depth(int new_max_depth)
{
  value_printer_max_depth = new_max_depth;
}

void
monda_set_value_printer_max_string_length(int new_max_string_length)
{
  value_printer_max_string_length = new_max_string_length;
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
