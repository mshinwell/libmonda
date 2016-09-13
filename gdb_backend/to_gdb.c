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
#include "value.h"

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

typedef enum {
  ARGUMENTS,
  LOCALS
} find_named_value_stage;

typedef struct {
  find_named_value_stage stage;
  struct frame_info* frame;
  const struct block* block;
  const char* name_to_find;
  struct value* found_value;
} find_named_value_data;

static void
find_named_value_callback(const char* name, struct symbol* sym,
      void* user_data)
{
  find_named_value_data* output = (find_named_value_data*) user_data;
  if (!output->found_value) {
    if (!strcmp(name, output->name_to_find)) {
      switch (output->stage) {
        case ARGUMENTS: {
          struct frame_arg arg;
          struct frame_arg entry_arg;
          read_frame_arg(sym, output->frame, &arg, &entry_arg);
          if (arg.val && arg.sym && !arg.error) {
            output->found_value = arg.val;
          }
          if (arg.error) {
            xfree(arg.error);
          }
          if (entry_arg.error) {
            xfree(entry_arg.error);
          }
          break;
        }

        case LOCALS: {
          struct value* local;
          local = read_var_value(sym, output->block, output->frame);
          if (local) {
            output->found_value = local;
          }
          break;
        }

        default:
          gdb_assert(0);
      }
    }
  }
}

CAMLprim value
monda_find_named_value(value v_name)
{
  /* Search arguments and local variables of the selected frame to find a
     value with the given name. */

  CAMLparam0();
  CORE_ADDR pc;
  static value *callback = NULL;
  struct frame_info* selected_frame;
  struct symbol* function;
  const struct block* block;
  find_named_value_data output;
  CAMLlocal2(v_found_value, v_dwarf_type);
  value v_option;

  selected_frame = get_selected_frame_if_set();
  if (selected_frame != NULL) {
    if (get_frame_pc_if_available(selected_frame, &pc)) {
      block = get_frame_block(selected_frame, NULL);
      if (block != NULL) {
        output.name_to_find = String_val(v_name);
        output.frame = selected_frame;
        output.block = block;
        output.found_value = NULL;
        output.stage = LOCALS;
        iterate_over_block_local_vars(output.block,
          find_named_value_callback, &output);
        if (!output.found_value) {
          function = get_frame_function(selected_frame);
          if (function != NULL) {
            block = SYMBOL_BLOCK_VALUE(function);
            output.block = block;
            output.stage = ARGUMENTS;
            iterate_over_block_arg_vars(output.block,
              find_named_value_callback, &output);
          }
        }
      }
    }
  }

  if (!output.found_value) {
    CAMLreturn(Val_unit);  /* None */
  }

  v_found_value = caml_copy_nativeint(value_as_address(output.found_value));
  v_dwarf_type = caml_copy_string(
    TYPE_NAME(value_type(output.found_value)));

  v_option = caml_alloc_small(2, 0 /* Found */);
  Field(v_option, 0) = v_found_value;
  Field(v_option, 1) = v_dwarf_type;

  CAMLreturn(v_option);
}

CAMLprim value
monda_find_global_symbol(value v_name)
{
  /* Search for a global "symbol" with the given name.  The name is one as
     written into DW_AT_name for toplevel constant and inconstant identifiers.
   */

  CAMLparam0();
  CAMLlocal2(v_found_value, v_dwarf_type);
  value v_option;
  struct block_symbol sym;
  uint64_t sym_value;

  sym = lookup_symbol(String_val(v_name), NULL, VAR_DOMAIN, NULL);

  if (!sym.symbol) {
    /*fprintf(stderr, "%s not found from gdb\n", String_val(v_name));*/
    CAMLreturn(Val_unit);  /* None */
  }

  switch (SYMBOL_CLASS(sym.symbol)) {
    case LOC_CONST:  /* Let_symbol / constant case */
      sym_value = SYMBOL_VALUE(sym.symbol);
      break;

    case LOC_COMPUTED: { /* Initialize_symbol / inconstant case */
      struct value* v;

      if (SYMBOL_COMPUTED_OPS(sym.symbol)->read_needs_frame(sym.symbol)
            || SYMBOL_COMPUTED_OPS(sym.symbol)->location_has_loclist) {
        /* We jolly well shouldn't need a frame; we should also not be
           PC-dependent (i.e. no location list). */
        CAMLreturn(Val_unit);
      }

      v = SYMBOL_COMPUTED_OPS(sym.symbol)->read_variable(sym.symbol, NULL);
      if (v == NULL) {
        CAMLreturn(Val_unit);
      }

      sym_value = (uint64_t) value_as_address(v);

      break;
    }

    default:
      CAMLreturn(Val_unit);
  }
/*
  fprintf(stderr, "%s found to have value %p, type %s\n",
          String_val(v_name),
          SYMBOL_VALUE_ADDRESS(sym.symbol),
          TYPE_NAME(SYMBOL_TYPE(sym.symbol)));*/

  v_found_value = caml_copy_nativeint(sym_value);
  v_dwarf_type = caml_copy_string(
    TYPE_NAME(SYMBOL_TYPE(sym.symbol)));

  v_option = caml_alloc_small(2, 0 /* Found */);
  Field(v_option, 0) = v_found_value;
  Field(v_option, 1) = v_dwarf_type;

  CAMLreturn(v_option);
}

CAMLprim value
monda_value_struct_elt(struct value* v, value field_name)
{
  struct value* found;

  found = value_struct_elt(&v, NULL, String_val(field_name), NULL,
    "libmonda error: field not found (monda_value_struct_elt)");

  return (value) found;
}
