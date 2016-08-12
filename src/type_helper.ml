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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let type_expr_and_env_from_dwarf_type ~dwarf_type ~cmt_cache
      ~cmt_file_search_path =
  Cmt_cache.clear_search_paths cmt_cache;
  Cmt_cache.set_primary_search_path cmt_cache cmt_file_search_path;
  let cmt_file_and_ident_name =
    match Name_laundry.split_base_type_die_name dwarf_type with
    | None -> None
    | Some { output_path; ident_name; ident_stamp; } ->
      let output_dir = Filename.dirname output_path in
      let source_file = Filename.basename output_path in
      let cmt_leafname =
        match Filename.chop_extension source_file with
        | basename -> basename ^ ".cmt"
        | exception (Invalid_argument _) -> source_file
      in
      let cmt =
        Cmt_cache.load cmt_cache
          ~leafname:cmt_leafname
          ~expected_in_directory:output_dir
      in
      match cmt with
      | None -> Printf.eprintf "cmt not found\n%!"; None
      | Some cmt ->
        Cmt_cache.set_secondary_search_path cmt_cache
          (Cmt_file.search_path cmt);
        Some (cmt, ident_name, ident_stamp)
  in
  match cmt_file_and_ident_name with
  | None -> None
  | Some (cmt_file, name, stamp) ->
    Cmt_file.type_of_ident cmt_file ~name ~stamp
