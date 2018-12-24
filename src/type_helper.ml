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

module Make (D : Debugger.S) = struct
  (* Search order for .cmi files:

     1. The combined "load paths" extracted from all .cmt files loaded thus
        far.  It is generally expected that a particular .cmi will only be
        in one of the directories thus specified; but in any event, order
        is maintained to some extent, according to the code below.

     2. Any other source path set in GDB.

     Search order for .cmt files:

     1. The "prefix name" directory (where the compiler wrote the compilation
        artifacts, possibly set using the "-o" option) as transmitted in
        the DWARF information for the corresponding compilation unit.

     2. The search order as for .cmi files, above.

     In all cases, for .cmi and .cmt searching and irrespective of where the
     path was obtained, any path used for lookup is subjected to any
     source path substitution rules set in GDB.  Such rules can be used to
     easily describe the relocation of a tree of source files and compiler
     artifacts after the build process.
  *)

  let add_to_load_path new_dirnames =
    let new_dirnames =
      List.filter (fun dirname -> not (List.mem dirname !our_load_path))
        new_dirnames
    in
    List.iter (fun dirname -> D.add_search_path ~dirname)
      (List.rev new_dirnames)

  let load_cmi ~unit_name =
    let filename = unit_name ^ ".cmi" in
    match D.find_and_open ~filename ~dirname:None with
    | None -> None
    | Some (filename, chan) ->
      let cmi = Env.read_cmi_from_channel chan in
      Some {
        filename;
        cmi;
      }

  let () =
    Env.Persistent_signature.load := load_cmi

  let type_expr_and_env_from_dwarf_type ~dwarf_type ~cmt_cache =
    let cmt_file_and_ident_name =
      match Name_laundry.split_base_type_die_name dwarf_type with
      | None -> None
      | Some { compilation_unit; ident_name; ident_stamp; } ->
        let unit_name = Ident.create_persistent compilation_unit in
        match D.ocaml_specific_compilation_unit_info ~unit_name with
        | None -> None
        | Some { prefix_name; _ } ->
          let filename = (Filename.basename prefix_name) ^ ".cmt" in
          let dirname = Filename.dirname prefix_name in
          match D.find_and_open ~filename ~dirname with
          | None -> None
          | Some cmt_chan ->
            let cmt =
              Cmt_cache.load_from_channel_then_close cmt_cache cmt_chan
            in
            match cmt with
            | None -> None
            | Some cmt ->
              add_to_our_load_path (Cmt_file.load_path cmt);
              Some (cmt, ident_name, ident_stamp)
    in
    match cmt_file_and_ident_name with
    | None -> None
    | Some (cmt_file, name, stamp) ->
      Cmt_file.type_of_ident cmt_file ~name ~stamp

  (* CR mshinwell: decide what to do about warning levels etc.
          Printf.eprintf ".cmt file %s in directory %s not found\n%!"
            cmt_leafname output_dir;
  *)
end
