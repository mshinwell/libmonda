/**************************************************************************/
/*                                                                        */
/*                Make OCaml native debugging awesome!                    */
/*                                                                        */
/*        Mark Shinwell and Frederic Bour, Jane Street Europe             */
/*                                                                        */
/* Copyright (c) 2013--2016 Jane Street Group, LLC                        */
/*                                                                        */
/* Permission is hereby granted, free of charge, to any person obtaining  */
/* a copy of this software and associated documentation files             */
/* (the "Software"), to deal in the Software without restriction,         */
/* including without limitation the rights to use, copy, modify, merge,   */
/* publish, distribute, sublicense, and/or sell copies of the Software,   */
/* and to permit persons to whom the Software is furnished to do so,      */
/* subject to the following conditions:                                   */
/*                                                                        */
/* The above copyright notice and this permission notice shall be         */
/* included in all copies or substantial portions of the Software.        */
/*                                                                        */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        */
/* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. */
/* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY   */
/* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,   */
/* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE      */
/* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                 */
/*                                                                        */
/**************************************************************************/

#define CAML_NAME_SPACE 1

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/../asmrun/stack.h>

#include "monda.h"
#include "ml_utils.h"

#include "../frame.h"
#include "../infcall.h"
#include "../stack.h"
#include "../symtab.h"
#include "../valprint.h"

#include <string.h>

extern value caml_make_vect (value, value);

static int debug(void)
{
  return ((getenv("MONDA_DEBUG")) != NULL);
}

int
monda_init (void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup (argv);
  return 1;
}

static void
ocaml_val_print (value* callback, struct type *type, const gdb_byte *valaddr,
                 int embedded_offset, CORE_ADDR address,
                 struct ui_file *stream, int recurse, const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  CAMLparam0();
  CAMLlocal4(v_type, v_stream, v_target);
  CAMLlocalN(args, 5);
  struct frame_info* selected_frame;

  gdb_assert(type != NULL && TYPE_NAME(type) != NULL);  /* enforced in ocaml-lang.c */
  v_type = caml_copy_string(TYPE_NAME(type));

  v_target = Val_target (*(CORE_ADDR*)valaddr);
  v_stream = Val_ptr(stream);

  Store_field(args, 0, v_target);
  Store_field(args, 1, v_stream);
  Store_field(args, 2, v_type);
  Store_field(args, 3, Val_bool(options->summary));
  (void) caml_callbackN (*callback, 4, args);

  CAMLreturn0;
}

void
monda_val_print (struct type *type, struct symbol *symbol,
                             const gdb_byte *valaddr,
                             int embedded_offset,
                             CORE_ADDR address, struct ui_file *stream,
                             int recurse, const struct value *val,
                             const struct value_print_options *options,
                             int depth)
{
  static value *callback = NULL;

  if (callback == NULL) {
    callback = caml_named_value ("monda_val_print");
  }

  if (callback != NULL) {
    ocaml_val_print (callback, type, symbol, valaddr, embedded_offset,
                     address, stream, recurse, val, options, depth);
  }
}

char*
monda_demangle (char* mangled, int options)
{
  static value *cb = NULL;
  CAMLparam0();
  CAMLlocal2 (caml_res, caml_mangled);

  char* res = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("monda_demangle");
  }

  if (cb != NULL) {
    caml_mangled = caml_copy_string (mangled);
    caml_res = caml_callback (*cb, caml_mangled);

    if (Is_block(caml_res)) {
      gdb_assert(Tag_val(caml_res) == 0 && Wosize_val(caml_res) == 1);
      res = strdup (String_val(Field(caml_res, 0)));
    }
  }

  CAMLreturnT (char*, res);
}

static int
compilation_directories_for_source_file_callback(struct symtab* symtab,
                                               void* directories_list_head)
{
  CAMLparam0();
  CAMLlocal1(v_dirname);

  if (symtab->dirname) {
    value v_list_cell;

    v_dirname = caml_copy_string(symtab->dirname);

    v_list_cell = caml_alloc_small(2, 0);
    Field(v_list_cell, 0) = v_dirname;
    Field(v_list_cell, 1) = *(value*) directories_list_head;

    *(value*) directories_list_head = v_list_cell;
  }

  CAMLreturnT(int, 0);  /* always continue the search */
}

value
gdb_ocaml_compilation_directories_for_source_file(value v_file)
{
  CAMLparam0();
  CAMLlocal1(v_directories_list);

  v_directories_list = Val_long(0);
  iterate_over_symtabs(String_val(v_file),
    &compilation_directories_for_source_file_callback,
    &v_directories_list);  /* take the address since we may cause a GC */

  CAMLreturn(v_directories_list);
}

/* These are initialized by way of [_initialize_ocaml_language]. */
static int value_printer_max_depth;
static char* search_path = NULL;

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
