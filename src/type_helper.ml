(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2013--2019 Jane Street Group, LLC                        *)
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
  let type_and_env_from_dwarf_type0 ~dwarf_type ~cmt_cache =
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

  let type_is_polymorphic env ty =
    Btype.fold_type_expr (fun is_poly ty ->
        is_poly || Btype.is_Tvar (Ctype.expand_head env ty))
      false ty

  let rec type_and_env_from_dwarf_type ~dwarf_type ~cmt_cache frame =
    if Monda_debug.debug then begin
      Printf.fprintf stdout "Checking DWARF type %s\n" dwarf_type
    end;
    match type_and_env_from_dwarf_type0 ~dwarf_type ~cmt_cache with
    | None ->
      if Monda_debug.debug then begin
        Printf.fprintf stdout "Couldn't get OCaml type + env\n"
      end;
      None
    | (Some (ty, env, is_parameter)) as result ->
      match ty with
      | Module _ -> result
      | Core ty ->
        let ty = Ctype.expand_head env ty in
        let normal_case () = Some (Cmt_file.Core ty, env, is_parameter) in
        if type_is_polymorphic env ty && not (D.Frame.inlined frame) then begin
          if Monda_debug.debug then begin
            Printf.fprintf stdout "Type is polymorphic.\n"
          end;
          match is_parameter with
          | Local ->
            if Monda_debug.debug then begin
              Printf.fprintf stdout "Not a parameter.\n"
            end;
            normal_case ()
          | Parameter { index; } ->
            if Monda_debug.debug then begin
              Printf.fprintf stdout
                "Type is polymorphic and that of a parameter.\n"
            end;
            match D.Frame.caller frame with
            | No_caller ->
              if Monda_debug.debug then begin
                Printf.fprintf stdout "Couldn't find caller frame.\n"
              end;
              normal_case ()
            | Caller (caller_frame, call_site) ->
              if Monda_debug.debug then begin
                Printf.fprintf stdout "Found caller frame.\n"
              end;
              match D.Call_site.dwarf_type_of_argument call_site ~index with
              | None ->
                (* The DWARF type may be absent if there was no identifier
                   named in the provenance of the corresponding argument
                   register.  In this case, try to use the location of the
                   call instruction instead, looking the type up using that
                   in the .cmt file. *)
                if Monda_debug.debug then begin
                  Printf.fprintf stdout "Couldn't find DWARF arg type.\n"
                end;
                begin match
                  D.Call_site.line_number call_site,
                    D.Call_site.column_number call_site
                with
                | None, None | None, Some _ | Some _, None ->
                  if Monda_debug.debug then begin
                    Printf.fprintf stdout "Couldn't find call site location.\n"
                  end;
                  normal_case ()
                | Some line, Some column ->
                  match
                    D.Call_site.ocaml_specific_compilation_unit_info call_site
                  with
                  | None ->
                    if Monda_debug.debug then begin
                      Printf.fprintf stdout "Couldn't find unit info.\n"
                    end;
                    normal_case ()
                  | Some unit_info ->
                    let comp_unit =
                      (* CR mshinwell: do something so this isn't here *)
                      Compilation_unit.create unit_info.unit_name
                        (Linkage_name.create (
                          Compilenv.make_symbol
                            ~unitname:(Ident.name unit_info.unit_name)
                            None))
                    in
                    match Cmt_cache.load cmt_cache comp_unit with
                    | None ->
                      if Monda_debug.debug then begin
                        Printf.fprintf stdout ".cmt load failed\n"
                      end;
                      normal_case ()
                    | Some call_site_cmt ->
                      match
                        Cmt_file.type_of_call_site_argument call_site_cmt ~line
                          ~column ~index
                      with
                      | None ->
                        if Monda_debug.debug then begin
                          Printf.fprintf stdout
                            "type lookup failed (line %d, column %d)\n"
                            line column
                        end;
                        normal_case ()
                      | Some (ty, env) ->
                        if Monda_debug.debug then begin
                          Printf.fprintf stdout
                            "type lookup ok (line %d, column %d)\n"
                            line column
                        end;
                        Some (ty, env, is_parameter)
                end
              | Some dwarf_type ->
                type_and_env_from_dwarf_type ~dwarf_type ~cmt_cache caller_frame
        end else begin
          if Monda_debug.debug then begin
            Printf.fprintf stdout "Type is not polymorphic.\n"
          end;
          normal_case ()
        end
end
