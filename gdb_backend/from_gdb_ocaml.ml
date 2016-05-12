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

module Our_value_printer = Value_printer.Make (Gdb_debugger)

let value_printer = Our_value_printer.create ~cmt_cache

let print_value ~addr ~(stream : Gdb_debugger.stream) ~dwarf_type ~summary
      ~max_depth ~cmt_file_search_path =
  (* When doing e.g. "inf reg", gdb passes "int64_t" as the type to print
     at.  Since we can't yet print out-of-heap values etc, don't try to
     be fancy here. *)
  match Name_laundry.split_base_type_die_name dwarf_type with
  | None -> false
  | Some _ ->
    let cmt_file_search_path =
      Misc.Stdlib.String.split cmt_file_search_path ~on:':'
    in
    Our_value_printer.print value_printer
      ~scrutinee:addr
      ~formatter:(Gdb_debugger.formatter stream)
      ~dwarf_type
      ~summary
      ~max_depth
      ~cmt_file_search_path;
    true

let demangle ~mangled_name =
  (* CR mshinwell: this needs revisiting. *)
  Some mangled_name

let () =
  Callback.register "From_gdb_ocaml.print_value" print_value;
  Callback.register "From_gdb_ocaml.demangle" demangle
