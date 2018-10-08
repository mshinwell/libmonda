(**************************************************************************)
(*                                                                        *)
(*                Make OCaml native debugging awesome!                    *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(* Copyright (c) 2013--2016 Jane Street Group, LLC                        *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this software and associated documentation files             *)
(* (the "Software"), to deal in the Software without restriction,         *)
(* including without limitation the rights to use, copy, modify, merge,   *)
(* publish, distribute, sublicense, and/or sell copies of the Software,   *)
(* and to permit persons to whom the Software is furnished to do so,      *)
(* subject to the following conditions:                                   *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. *)
(* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY   *)
(* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,   *)
(* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE      *)
(* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                 *)
(*                                                                        *)
(**************************************************************************)

module T = Typedtree

let debug = Monda_debug.debug
let print_path = Monda_debug.print_path

type t = {
  cmt_cache : Cmt_cache.t;
}

let create ~cmt_cache =
  { cmt_cache;
  }

let rec find_module_binding t ~dir_prefix ~path ~is_toplevel ~env =
  if debug then
    Printf.printf "find_module_binding: 1. path=%s\n%!" (print_path path);
  let path = Env.normalize_path None env path in
  let original_path = path in
  if debug then
    Printf.printf "find_module_binding: 2. path=%s\n%!" (print_path path);
  match path with
  | Path.Pident ident ->
    let unit_name = Ident.name ident in
    if debug then Printf.printf "trying to read cmt for %s\n%!" unit_name;
    let leafname = (String.lowercase_ascii unit_name) ^ ".cmt" in
    let cmt = Cmt_cache.load t.cmt_cache ~leafname in
    if debug then Printf.printf "Cmt_cache.load finished\n%!";
    begin match cmt with
    | None ->
      if debug then Printf.printf "cmt read failed\n%!";
      `Not_found
    | Some cmt ->
      begin match Cmt_file.cmt_infos cmt with
      | Some cmt ->
        if debug then Printf.printf "cmt read was successful\n%!";
        begin match cmt.Cmt_format.cmt_annots with
        | Cmt_format.Implementation structure ->
          let mod_binding =
            { T.
              mb_id = ident;
              mb_name = Location.mkloc (Ident.name ident) Location.none;
              mb_expr =
                { T.
                  mod_desc = T.Tmod_structure structure;
                  mod_loc = Location.none;
                  mod_type = Types.Mty_ident path;  (* bogus, but unused *)
                  mod_env = env;  (* likewise *)
                  mod_attributes = [];
                };
              mb_attributes = [];
              mb_loc = Location.none;
            }
          in
          `Found_module mod_binding
        | Cmt_format.Packed (signature, _) ->
          (* We look for all .cmt files in the same directory at the moment.  Of
             course, with packing, there may be name clashes.  Perhaps we can
             avoid those by not supporting packing in the future. *)
          `Found_pack ident
        | Cmt_format.Interface _
        | Cmt_format.Partial_implementation _
        | Cmt_format.Partial_interface _ ->
          (* CR mshinwell: no idea what to do with these *)
          `Not_found
        end
      | None ->
        if debug then Printf.printf "cmt read not useful\n%!";
        `Not_found
      end
    end
  | Path.Pdot (path, component, _) ->
    if debug then Printf.printf "path %s, component %s\n%!"
      (print_path path) component;
    let binding =
      find_module_binding t ~dir_prefix ~path ~is_toplevel:false ~env
    in
    begin match binding with
    | `Not_found -> `Not_found
    | `Found_type_decl _ -> assert false
    | `Found_pack ident ->
      let path = Path.Pident (Ident.create_local component) in
      (* [`Found_pack] means that we found a packed module.  In this case, we
         look for the .cmt of the next module down the path in a subdirectory
         corresponding to the packed module's name. *)
      let dir_prefix =
        let name = String.lowercase_ascii (Ident.name ident) in
        match dir_prefix with
        | None -> Some name
        | Some prefix -> Some (Filename.concat prefix name)
      in
      find_module_binding t ~dir_prefix ~path ~is_toplevel:false ~env
    | `Found_module mod_binding ->
      if debug then
        Printf.printf
          "find_module_binding: Found_module case (path %s, toplevel? %s)\n%!"
          (print_path path) (if is_toplevel then "yes" else "no");
      let rec examine_mod_desc mod_desc =
        match mod_desc with
        | T.Tmod_structure structure ->
          let rec traverse_structure ~structure_items =
            match structure_items with
            | [] -> `Not_found
            | structure_item::structure_items ->
              let rec traverse_modules ~mod_bindings =
                match mod_bindings with
                | [] -> `Not_found
                | mod_binding::mod_bindings ->
                  if debug then
                    Printf.printf "checking component '%s'... "
                      (Ident.name mod_binding.T.mb_id);
                  if (Ident.name mod_binding.T.mb_id) = component then begin
                    if debug then Printf.printf "matches\n%!";
                    `Found_module mod_binding
                  end else begin
                    if debug then Printf.printf "does not match\n%!";
                    traverse_modules ~mod_bindings
                  end
              in
              match structure_item.T.str_desc with
              | T.Tstr_type (_rec_flag, type_decls) when is_toplevel ->
                let rec traverse_type_decls ~type_decls =
                  match type_decls with
                  | [] -> traverse_structure ~structure_items
                  | type_decl::type_decls ->
                    if debug then
                      Printf.printf "checking type decl '%s'... "
                        (Ident.unique_name type_decl.T.typ_id);
                    if (Ident.name type_decl.T.typ_id) = component then
                      let type_decl_env =
                        (* CR mshinwell: this might rely on globals---we should
                           clean this all up *)
                        try
                          (* We return the [str_final_env] so we have all of
                             the definitions from the structure available for
                             lookup (they may be needed for recursive
                             cases).  By contrast the [structure_item]'s
                             [str_env] only has the environment immediately
                             before the definition. *)
                          Env.env_of_only_summary Envaux.env_from_summary
                            structure.T.str_final_env
                        with Envaux.Error _ -> Env.empty
(*
                        match type_decl.T.typ_manifest with
                        | None ->
                          Printf.printf "typ_manifest is None\n%!";
                        (*  Env.empty*)
                          mod_binding.T.mb_expr.T.mod_env
                        | Some manifest ->
                          Printf.printf "typ_manifest is Some\n%!";
                          mod_binding.T.mb_expr.T.mod_env
                        (*  manifest.T.ctyp_env *)
*)
                      in
                      `Found_type_decl (original_path, type_decl, type_decl_env)
                    else
                      traverse_type_decls ~type_decls
                in
                traverse_type_decls ~type_decls
              | T.Tstr_module mod_binding when not is_toplevel ->
                traverse_modules ~mod_bindings:[mod_binding]
              | T.Tstr_recmodule mod_bindings when not is_toplevel ->
                traverse_modules ~mod_bindings
              | _ -> traverse_structure ~structure_items
          in
          traverse_structure ~structure_items:structure.T.str_items
        | T.Tmod_ident (path, _) ->
          (* This whole function is called when we're trying to find the manifest
             type for an abstract type.  As such, even if we get here and could
             look up types via [path] in the environment, we don't---it
             may well yield another abstract type.  Instead we go straight to
             the implementation, having used the environment only to normalize
             the path. *)
          (* XXX this probably isn't right; we're assuming [path] starts from
             the toplevel.  Should we prepend our [path] so far (from Pdot)?
             Then what happens if it was absolute? *)
          if debug then
            Printf.printf "find_module_binding: 3. path=%s\n%!"
              (print_path path);
          find_module_binding t ~dir_prefix ~path ~is_toplevel:false
            ~env
        (* CR mshinwell: cope with these *)
        | T.Tmod_functor _ ->
          if debug then Printf.printf "Tmod_functor\n%!";
          `Not_found
        | T.Tmod_apply _ ->
          if debug then Printf.printf "Tmod_apply\n%!";
          `Not_found
        | T.Tmod_constraint
            (mod_expr, _mod_type, _mod_type_constraint, _mod_coercion) ->
          if debug then Printf.printf "Tmod_constraint\n%!";
          examine_mod_desc mod_expr.T.mod_desc
        | T.Tmod_unpack _ ->
          if debug then Printf.printf "Tmod_unpack\n%!";
          `Not_found
      in
      examine_mod_desc mod_binding.T.mb_expr.T.mod_desc
    end
  | Path.Papply (path1, path2) ->
    `Not_found  (* CR mshinwell: handle this case *)

(* CR mshinwell: we should maybe try lookups via the environment from higher
   up the tree. *)

let print_env env =
  Env.fold_types (fun name path _ () ->
    Printf.printf "print_env: name=%s path=%s\n%!" name (print_path path))
    None env ()

let find_manifest_of_abstract_type t ~formatter ~path ~env =
  if debug then
    Printf.printf "finding abstract type: %s\n%!" (print_path path);
  (* [path] must identify a type declaration.  However, watch out---it may be
     unqualified. *)
  (* XXX how do we cope with the unqualified cases?  Built-in types are one,
     but presumably there may be others?  Not sure. *)
  match path with
  | Path.Pident _ -> None
  | Path.Pdot _ | Path.Papply _ ->
    let binding =
      find_module_binding t ~dir_prefix:None ~path ~is_toplevel:true ~env
    in
    match binding with
    | `Not_found | `Found_pack _ ->
      if debug then Printf.printf "find_manifest: Not_found or pack\n%!";
      None
    | `Found_module _ -> assert false
    | `Found_type_decl (path, type_decl, type_decl_env) ->
      if debug then begin
        Printf.printf "find_manifest: type decl found.  type_decl_env is:\n%!";
        print_env type_decl_env;
        Printf.printf "... and env is:\n%!";
        print_env env;
        Printtyp.type_declaration (Ident.create_persistent "foo")
          formatter type_decl.T.typ_type;
        Format.print_flush ()
      end;
      Some (path, type_decl.T.typ_type, type_decl_env)
(*
val fold_types:
  (string -> Path.t -> type_declaration * type_descriptions -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
*)
(*
        let env = structure.T.str_final_env in
        Env.iter_types (fun path1 (path2, (decl, _)) ->
          Format.fprintf formatter "path1=%s path2=%s decl=\n"
            (print_path path1) (print_path path2);
          Printtyp.type_declaration ident formatter decl
        ) env;
        ...this just gives us the abstract type again!
        try begin
          if debug then Printf.printf "find_manifest: env lookup OK\n%!";
          let decl = Env.find_type path env in
          Some decl
        end
        with Not_found -> begin
          if debug then Printf.printf "find_manifest: env lookup failed\n%!";
          None
        end
*)
