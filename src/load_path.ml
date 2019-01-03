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

module String = Misc.Stdlib.String

module Make (D : Debugger.S) = struct
  let our_load_path = ref String.Set.empty

  let add_to_load_path new_dirnames =
    let new_dirnames =
      List.filter (fun dirname -> not (String.Set.mem dirname !our_load_path))
        new_dirnames
    in
    List.iter (fun dirname ->
        D.add_search_path ~dirname;
        if Monda_debug.debug then begin
          Format.eprintf "Adding %s to debugger's search path\n%!" dirname;
        end;
        our_load_path := String.Set.add dirname !our_load_path)
      (List.rev new_dirnames)

  let load_cmi ~unit_name =
    let filename = (String.uncapitalize_ascii unit_name) ^ ".cmi" in
    if Monda_debug.debug then begin
      Format.eprintf "Trying to load .cmi file: %s\n%!" filename;
    end;
    match D.find_and_open ~filename ~dirname:None with
    | None -> None
    | Some (filename, chan) ->
      let cmi = Cmi_format.read_cmi_from_channel ~filename chan in
      Some ({
        filename;
        cmi;
      } : Env.Persistent_signature.t)

  let () =
    Env.Persistent_signature.load := load_cmi

  let load_cmt compilation_unit =
    let unit_name = Compilation_unit.get_persistent_ident compilation_unit in
    match D.ocaml_specific_compilation_unit_info ~unit_name with
    | None ->
      if Monda_debug.debug then begin
        Format.eprintf "Couldn't get OCaml-specific CU info for %a"
          Ident.print unit_name
      end;
      None
    | Some { prefix_name; _ } ->
      let filename = (Filename.basename prefix_name) ^ ".cmt" in
      let dirname = Filename.dirname prefix_name in
      match D.find_and_open ~filename ~dirname:(Some dirname) with
      | None ->
        if Monda_debug.debug then begin
          Printf.eprintf ".cmt file %s could not be found/opened by \
              debugger\n%!"
            filename;
        end;
        None
      | Some (filename, cmt_chan) ->
        if Monda_debug.debug then begin
          Printf.eprintf ".cmt file %s found by debugger and opened\n%!"
            filename;
        end;
        Cmt_file.load_from_channel_then_close ~filename cmt_chan
          ~add_to_load_path
end
