(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2013--2018 Jane Street Group, LLC                        *)
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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (D : Debugger.S) (Cmt_cache : Cmt_cache_intf.S) = struct
  let type_and_env_from_dwarf_type ~dwarf_type ~cmt_cache =
    let cmt_file_and_ident_name =
      match Dwarf_name_laundry.split_base_type_die_name dwarf_type with
      | None -> None
      | Some { compilation_unit; ident_name; ident_stamp; is_parameter; } ->
        let compilation_unit =
          (* CR mshinwell: move to [Dwarf_name_laundry] *)
          Compilation_unit.create (Ident.create_persistent compilation_unit)
            (Linkage_name.create (
              Compilenv.make_symbol ~unitname:compilation_unit None))
        in
        match Cmt_cache.load cmt_cache compilation_unit with
        | None -> None
        | Some cmt -> Some (cmt, ident_name, ident_stamp, is_parameter)
    in
    match cmt_file_and_ident_name with
    | None -> None
    | Some (cmt_file, name, stamp, is_parameter) ->
      match Cmt_file.type_of_ident cmt_file ~name ~stamp with
      | None -> None
      | Some (ty, env) -> Some (ty, env, is_parameter)

  (* CR mshinwell: decide what to do about warning levels etc.
          Printf.eprintf ".cmt file %s in directory %s not found\n%!"
            cmt_leafname output_dir;
  *)
end
