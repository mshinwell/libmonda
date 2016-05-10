/**************************************************************************/
/*                                                                        */
/*                Make OCaml native debugging awesome!                    */
/*                                                                        */
/*                  Mark Shinwell, Jane Street Europe                     */
/*                  with assistance from Frederic Bour                    */
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

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include "defs.h"
#include "symtab.h"

static int
compilation_directories_for_source_file_callback(struct symtab* symtab,
                                                 void* directories_list_head)
{
  CAMLparam0();
  CAMLlocal1(v_dirname);

  if (symtab->compunit_symtab->dirname) {
    value v_list_cell;

    v_dirname = caml_copy_string(symtab->compunit_symtab->dirname);

    v_list_cell = caml_alloc_small(2, 0);
    Field(v_list_cell, 0) = v_dirname;
    Field(v_list_cell, 1) = *(value*) directories_list_head;

    *(value*) directories_list_head = v_list_cell;
  }

  CAMLreturnT(int, 0);  /* always continue the search */
}

CAMLprim value
monda_compilation_directories_for_source_file(value v_file)
{
  CAMLparam0();
  CAMLlocal1(v_directories_list);

  v_directories_list = Val_long(0);
  iterate_over_symtabs(String_val(v_file),
    &compilation_directories_for_source_file_callback,
    &v_directories_list);  /* take the address since we may cause a GC */

  CAMLreturn(v_directories_list);
}

CAMLprim value
monda_find_pc_line (CORE_ADDR addr, int not_current)
{
  CAMLparam0();
  value result;
  CAMLlocal3(v_line, v_filename, v_filename_and_line);

  struct symtab_and_line sal;
  sal = find_pc_line(addr, not_current);

  if (!sal.symtab) {
    CAMLreturn(Val_long(0) /* None */);
  }

  v_filename = caml_copy_string(sal.symtab->filename);

  if (sal.line != 0) {
    v_line = caml_alloc_small(1, 0);
    Field(v_line, 0) = Val_long(sal.line);
  }
  else {
    v_line = Val_unit;
  }

  v_filename_and_line = caml_alloc_small(2, 0);
  Field(v_filename_and_line, 0) = v_filename;
  Field(v_filename_and_line, 1) = v_line;

  result = caml_alloc_small(1, 0 /* Some */);
  Field(result, 0) = v_filename_and_line;

  CAMLreturn(result);
}
