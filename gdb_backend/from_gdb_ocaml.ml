(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2013--2016 Jane Street Group, LLC                        *)
(*                                                                         *)
(*  Permission is hereby granted, free of charge, to any person obtaining  *)
(*  a copy of this software and associated documentation files             *)
(*  (the "Software"), to deal in the Software without restriction,         *)
(*  including without limitation the rights to use, copy, modify, merge,   *)
(*  publish, distribute, sublicense, and/or sell copies of the Software,   *)
(*  and to permit persons to whom the Software is furnished to do so,      *)
(*  subject to the following conditions:                                   *)
(*                                                                         *)
(*  The above copyright notice and this permission notice shall be         *)
(*  included in all copies or substantial portions of the Software.        *)
(*                                                                         *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     *)
(*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. *)
(*  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY   *)
(*  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,   *)
(*  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE      *)
(*  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                 *)
(*                                                                         *)
(***************************************************************************)

let cmt_cache = Cmt_cache.create ()

module Gdb_debugger_with_traversal = Unified_value_traversal.Make (Gdb_debugger)

module Follow_path = Follow_path.Make (Gdb_debugger_with_traversal)
module Our_value_printer = Value_printer.Make (Gdb_debugger_with_traversal)

let follow_path = Follow_path.create ~cmt_cache
let value_printer = Our_value_printer.create ~cmt_cache

let split_search_path path =
  String.split_on_char ':' path

let print_value is_synthetic
      (address_on_target : Gdb_debugger_with_traversal.Obj.t)
      (struct_value : Gdb_debugger_with_traversal.Synthetic_ptr.t)
      (stream : Gdb_debugger.stream) dwarf_type summary
      max_depth cmt_file_search_path =
  (* When doing e.g. "inf reg", gdb passes "int64_t" as the type to print
     at.  Since we can't yet print out-of-heap values etc, don't try to
     be fancy here. *)
  let can_print =
    Name_laundry.split_base_type_die_name dwarf_type <> None
      || Cmt_cache.find_cached_type cmt_cache ~cached_type:dwarf_type
           <> None
  in
  if not can_print then begin
    false
  end else begin
    let cmt_file_search_path = split_search_path cmt_file_search_path in
    let scrutinee =
      let module V = Gdb_debugger_with_traversal.Value in
      if is_synthetic then V.create_synthetic_ptr struct_value
      else V.create_exists_on_target address_on_target
    in
Format.eprintf "From_gdb_ocaml.print_value OVP starting.  Scrutinee %a.  \
  Synthetic? %b\n%!"
  Gdb_debugger_with_traversal.Value.print scrutinee
  is_synthetic;
    Our_value_printer.print value_printer
      ~scrutinee
      ~formatter:(Gdb_debugger.formatter stream)
      ~dwarf_type
      ~summary
      ~max_depth
      ~cmt_file_search_path;
    true
  end

type evaluate_result =
  | Failure
  | Ok of { rvalue : Gdb_debugger_with_traversal.Obj.t; type_name : string; }

let evaluate (path : string) (cmt_file_search_path : string)
      (stream : Gdb_debugger.stream) : evaluate_result =
  let cmt_file_search_path = split_search_path cmt_file_search_path in
  match
    Follow_path.evaluate follow_path ~path ~must_be_mutable:false
      ~cmt_file_search_path ~formatter:(Gdb_debugger.formatter stream)
      ~lvalue_or_rvalue:Follow_path.Rvalue
  with
  | None -> Failure
  | Some (rvalue, type_expr, env) ->
    let type_name = Cmt_cache.cache_type cmt_cache ~type_expr ~env in
    Ok { rvalue; type_name; }

type parse_result =
  | Success
  | Failure

let parse (path : string) : parse_result =
  if Follow_path.path_looks_ok ~path then Success else Failure

let demangle ~mangled_name =
  (* CR mshinwell: this needs revisiting. *)
  Some mangled_name

let () =
  Callback.register "From_gdb_ocaml.print_value" print_value;
  Callback.register "From_gdb_ocaml.demangle" demangle;
  Callback.register "From_gdb_ocaml.evaluate" evaluate;
  Callback.register "From_gdb_ocaml.parse" parse
