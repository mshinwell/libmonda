/**************************************************************************/
/*                                                                        */
/*                Make OCaml native debugging awesome!                    */
/*                                                                        */
/*                  Mark Shinwell, Jane Street Europe                     */
/*                  with assistance from Frederic Bour                    */
/*                                                                        */
/* Copyright (c) 2013--2018 Jane Street Group, LLC                        */
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

/* CR mshinwell: Unclear why this is needed.  The -I paths and the #includes
   here match ocaml-lang.c in the gdb tree, which does not need this. */
#define _GL_ATTRIBUTE_FORMAT_PRINTF(a, b)

/* gdb headers must be included before any system headers. */

#include "defs.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "expression.h"
#include "parser-defs.h"
#include "symtab.h"
#include "language.h"
#include "c-lang.h"
#include "gdb_assert.h"
#include "ocaml-lang.h"
#include "target.h"
#include "valprint.h"
#include "breakpoint.h"
#include "arch-utils.h"
#include "gdbcmd.h"
#include "varobj.h"
#include "stack.h"

#include <ctype.h>
#include <stdio.h>

#define CAML_NAME_SPACE 1
#define CAML_DO_NOT_TYPEDEF_VALUE 1

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

/* Remember: No gdb exceptions may escape from the functions called from OCaml
   in this file.  (See from_gdb.c for an explanation.)

   Functions here that register roots or catch gdb exceptions are structured so
   that there is a single return point.  This avoids any question about
   whether returns are permissible inside the try/catch blocks and ensures
   that we do not need to use [CAMLdrop].
*/

class compilation_directories_collector {
private:
  caml_value* v_directories_list_head;

public:
  compilation_directories_collector(caml_value* v_directories_list_head)
    : v_directories_list_head(v_directories_list_head)
  {
  }

  bool operator() (symtab *symtab)
  {
    CAMLparam0();
    CAMLlocal1(v_dirname);

    if (symtab->compunit_symtab->dirname) {
      caml_value v_list_cell;

      v_dirname = caml_copy_string(symtab->compunit_symtab->dirname);

      v_list_cell = caml_alloc_small(2, 0);
      Field(v_list_cell, 0) = v_dirname;
      Field(v_list_cell, 1) = *v_directories_list_head;

      *v_directories_list_head = v_list_cell;
    }

    CAMLreturnT(bool, false /* continue the search */);
  }
};

extern "C" caml_value
monda_compilation_directories_for_source_file(caml_value v_file)
{
  CAMLparam0();
  CAMLlocal1(v_directories_list);

  TRY {
    v_directories_list = Val_long(0);

    /* We take the address of [v_directories_list] since we may
       cause a GC during the iteration. */
    compilation_directories_collector collector(&v_directories_list);

    iterate_over_symtabs(String_val(v_file), collector);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    v_directories_list = Val_long(0);
  }
  END_CATCH

  CAMLreturn(v_directories_list);
}

extern "C" caml_value
monda_find_pc_line (CORE_ADDR addr, int not_current)
{
  CAMLparam0();
  caml_value result;
  CAMLlocal3(v_line, v_filename, v_filename_and_line);

  TRY {
    struct symtab_and_line sal;
    sal = find_pc_line(addr, not_current);

    if (!sal.symtab) {
      result = Val_long(0); /* None */
    }
    else {
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
    }
  }
  CATCH (ex, RETURN_MASK_ALL) {
    result = Val_long(0);  /* None */
  }
  END_CATCH

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

extern "C" caml_value
monda_find_named_value(caml_value v_name)
{
  /* Search arguments and local variables of the selected frame to find a
     value with the given name. */

  CAMLparam0();
  CORE_ADDR pc;
  static caml_value *callback = NULL;
  struct frame_info* selected_frame;
  struct symbol* function;
  const struct block* block;
  find_named_value_data output;
  CAMLlocal2(v_found_value, v_dwarf_type);
  caml_value v_option;
  CORE_ADDR address;
  struct type* type;
  int failed = 0;

  TRY {
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
    if (output.found_value) {
      address = value_as_address(output.found_value);
      type = value_type(output.found_value);
    }
  }
  CATCH (ex, RETURN_MASK_ALL) {
    failed = 1;
  }
  END_CATCH

  if (failed || !output.found_value) {
    CAMLreturn(Val_unit);  /* None */
  }

  v_found_value = caml_copy_nativeint(address);
  v_dwarf_type = caml_copy_string(TYPE_NAME(type) ? TYPE_NAME(type) : "");

  v_option = caml_alloc_small(2, 0 /* Found */);
  Field(v_option, 0) = v_found_value;
  Field(v_option, 1) = v_dwarf_type;

  CAMLreturn(v_option);
}

extern "C" caml_value
monda_find_global_symbol(caml_value v_name)
{
  /* Search for a global "symbol" with the given name.  The name is one as
     written into DW_AT_name for toplevel constant and inconstant identifiers.
   */

  CAMLparam0();
  CAMLlocal2(v_found_value, v_dwarf_type);
  caml_value v_option;
  struct block_symbol sym;
  uint64_t sym_value;

  TRY {
    sym = lookup_symbol(String_val(v_name), NULL, VAR_DOMAIN, NULL);

    if (!sym.symbol) {
      /*fprintf(stderr, "%s not found from gdb\n", String_val(v_name));*/
      v_option = Val_unit;  /* None */
    }
    else {
      int failed = 0;

      switch (SYMBOL_CLASS(sym.symbol)) {
        case LOC_CONST:  /* Let_symbol / constant case */
          sym_value = SYMBOL_VALUE(sym.symbol);
          break;

        case LOC_COMPUTED: { /* Initialize_symbol / inconstant case */
          struct value* v;
          symbol_needs_kind needs;

          needs = SYMBOL_COMPUTED_OPS(sym.symbol)
            ->get_symbol_read_needs(sym.symbol);

          if (needs == SYMBOL_NEEDS_FRAME
                || SYMBOL_COMPUTED_OPS(sym.symbol)->location_has_loclist) {
            /* We jolly well shouldn't need a frame; we should also not be
               PC-dependent (i.e. no location list). */
            failed = 1;
          }

          v = SYMBOL_COMPUTED_OPS(sym.symbol)->read_variable(sym.symbol, NULL);
          if (v == NULL) {
            failed = 1;
          }
          else {
            sym_value = (uint64_t) value_as_address(v);
          }

          break;
        }

        default:
          failed = 1;
          break;
      }
    /*
      fprintf(stderr, "%s found to have value %p, type %s\n",
              String_val(v_name),
              SYMBOL_VALUE_ADDRESS(sym.symbol),
              TYPE_NAME(SYMBOL_TYPE(sym.symbol)));*/

      if (failed) {
        v_option = Val_unit;  /* None */
      }
      else {
        v_found_value = caml_copy_nativeint(sym_value);
        v_dwarf_type = caml_copy_string(
          TYPE_NAME(SYMBOL_TYPE(sym.symbol)));

        v_option = caml_alloc_small(2, 0 /* Found */);
        Field(v_option, 0) = v_found_value;
        Field(v_option, 1) = v_dwarf_type;
      }
    }
  }
  CATCH (ex, RETURN_MASK_ALL) {
    v_option = Val_unit;  /* None */
  }
  END_CATCH

  CAMLreturn(v_option);
}

extern "C" caml_value
monda_value_struct_elt(struct value* v, caml_value field_name)
{
  struct value* found;

  TRY {
    found = value_struct_elt(&v, NULL, String_val(field_name), NULL,
      "libmonda error: field not found (monda_value_struct_elt)");
  }
  CATCH (ex, RETURN_MASK_ALL) {
    found = (struct value*) NULL;
  }
  END_CATCH

  return (caml_value) found;
}

static const uint64_t zero = (uint64_t) 0;

extern "C" const gdb_byte*
monda_value_contents(struct value* v)
{
  const gdb_byte* contents;

  TRY {
    contents = value_contents(v);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    contents = (const gdb_byte*) &zero;
  }
  END_CATCH

  return contents;
}

extern "C" int
monda_target_read_memory(CORE_ADDR memaddr, gdb_byte* myaddr,
                         ssize_t len)
{
  int retval;

  TRY {
    retval = target_read_memory(memaddr, myaddr, len);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    retval = -1;
  }
  END_CATCH

  return retval;
}

extern "C" caml_value
monda_fputs_filtered(const char* msg, struct ui_file* file)
{
  TRY {
    fputs_filtered(msg, file);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    do {} while (0);
  }
  END_CATCH

  return Val_unit;
}

extern "C" int
monda_find_pc_partial_function(CORE_ADDR addr, const char** fun_name,
                               CORE_ADDR* start_addr, CORE_ADDR* end_addr)
{
  int retval;

  TRY {
    retval = find_pc_partial_function(addr, fun_name, start_addr, end_addr);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    retval = -1;
  }
  END_CATCH

  return retval;
}

extern "C" int
monda_value_bits_synthetic_pointer(const struct value* value,
                                   int offset, int length)
{
  int retval;

  TRY {
    retval = value_bits_synthetic_pointer(value, offset, length);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    retval = -1;
  }
  END_CATCH

  return retval;
}

extern "C" LONGEST
monda_value_as_long(struct value* value)
{
  LONGEST retval;

  TRY {
    retval = value_as_long(value);
  }
  CATCH (ex, RETURN_MASK_ALL) {
    retval = (LONGEST) 0;
  }
  END_CATCH

  return retval;
}
