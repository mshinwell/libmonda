(**************************************************************************)
(*                                                                        *)
(*                Make OCaml native debugging awesome!                    *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(* Copyright (c) 2013--2018 Jane Street Group, LLC                        *)
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

[@@@ocaml.warning "+a-4-30-40-41-42"]

let debug = Monda_debug.debug

let distinguished_var_name = "camlaverydistinguishedvariableindeed"

module LocTable = Map.Make (struct
  type t = Location.t
  let compare x y =
    let lnum x = x.Location.loc_start.Lexing.pos_lnum in
    let cnum x = x.Location.loc_start.Lexing.pos_cnum in
    let start = compare (lnum x) (lnum y) in
    if start = 0 then compare (cnum x) (cnum y) else start
end)

module List = ListLabels

module String = struct
  include String
  module Map = Map.Make (String)
end

module T = Typedtree

type t = {
  cmt_infos : Cmt_format.cmt_infos;
  (* CR mshinwell: we can almost certainly do better than a map from
      every identifier (at least in common cases). *)
  (* CR trefis for mshinwell: in ocp-index, they use a trie from names to
      locations, you might want to do the same (but for types instead of
      positions, ofc) here. *)
  idents_to_types : (Types.type_expr * Env.t) String.Map.t;
}

let rec process_pattern ~(pat : T.pattern) ~idents_to_types =
  match pat.pat_desc with
  | Tpat_var (ident, _loc) ->
(*
    if debug then begin
      Printf.printf "process_pattern: Tpat_var %s\n%!" (Ident.unique_name ident)
    end;
*)
    String.Map.add (Ident.unique_name ident)
      (pat.pat_type, pat.pat_env)
      idents_to_types
  | Tpat_alias (pat, ident, _loc) ->
    let idents_to_types =
      String.Map.add (Ident.unique_name ident)
        (pat.pat_type, pat.pat_env)
        idents_to_types
    in
    process_pattern ~pat ~idents_to_types
  | Tpat_tuple pats
  | Tpat_construct (_, _, pats)
  | Tpat_array pats ->
    List.fold_left pats
      ~init:idents_to_types
      ~f:(fun idents_to_types pat ->
            process_pattern ~pat ~idents_to_types)
  | Tpat_variant (_label, pat_opt, _row_desc) ->
    begin match pat_opt with
    | None -> idents_to_types
    | Some pat -> process_pattern ~pat ~idents_to_types
    end
  | Tpat_record (loc_desc_pat_list, _closed) ->
    List.fold_left loc_desc_pat_list
      ~init:idents_to_types
      ~f:(fun idents_to_types (_loc, _desc, pat) ->
            process_pattern ~pat ~idents_to_types)
  | Tpat_or (pat1, pat2, _row_desc) ->
    process_pattern ~pat:pat1
      ~idents_to_types:(process_pattern ~pat:pat2 ~idents_to_types)
  | Tpat_lazy pat
  | Tpat_exception pat ->
    process_pattern ~pat ~idents_to_types
  | Tpat_any
  | Tpat_constant _ -> idents_to_types

and process_expression ~(exp : T.expression)
      ((idents_to_types, app_points) as init) =
  match exp.exp_desc with
  | Texp_let (_rec, value_binding, exp) ->
    let acc = process_value_binding ~value_binding init in
    process_expression ~exp acc
  | Texp_function { arg_label = _; param = ident; cases; partial = _; } ->
    let idents_to_types =
      (* The types of all the cases must be the same, so just use the first
         case. *)
      match cases with
      | case::_ ->
        String.Map.add (Ident.unique_name ident)
          (case.T.c_lhs.T.pat_type,
            case.T.c_lhs.T.pat_env)
          idents_to_types
      | _ -> idents_to_types
    in
    process_cases ~cases (idents_to_types, app_points)
  | Texp_apply (exp, args) ->
(*
    (* CR mshinwell: what happens when [exp] has already been partially
       applied? *)
    let app_points =
      let lst =
        List.map args ~f:(fun (label, expr_opt, _optional) ->
          match expr_opt with
          | None -> `Recover_label_ty label
          | Some e -> `Ty e.exp_type
        )
      in
      LocTable.add (exp.exp_loc) (lst, exp.exp_env) app_points
    in
*)
    let init = process_expression ~exp (idents_to_types, app_points) in
    List.fold_left args ~init ~f:(fun acc (_label, expr_opt) ->
      match expr_opt with
      | None -> acc
      | Some exp -> process_expression ~exp acc
    )
  | Texp_match (exp, cases, _) ->
    let acc = process_expression ~exp init in
    process_cases ~cases acc
  | Texp_try (exp, cases) ->
    let acc = process_expression ~exp init in
    process_cases ~cases acc
  | Texp_construct (_, _, expr_list)
    (* Texp_co... (loc, descr, list, "explicit arity":bool)
      * Note: that last bool disappeared on trunk. *)
  | Texp_array expr_list
  | Texp_tuple expr_list ->
    List.fold_left expr_list ~init ~f:(fun acc exp -> process_expression ~exp acc)
  | Texp_variant (_, Some exp) -> process_expression ~exp init
  | Texp_record { fields; extended_expression; _ } ->
    let acc =
      match extended_expression with
      | None -> init
      | Some exp -> process_expression ~exp init
    in
    Array.fold_left
      (fun acc
           (_, (record_label_definition : T.record_label_definition)) ->
        match record_label_definition with
        | Kept _ -> acc
        | Overridden (_, exp) -> process_expression ~exp acc)
      acc
      fields
  | Texp_ifthenelse (e1, e2, e_opt) ->
    let acc = process_expression ~exp:e1 init in
    let acc = process_expression ~exp:e2 acc in
    begin match e_opt with
    | None -> acc
    | Some exp -> process_expression ~exp acc
    end
  | Texp_sequence (e1, e2)
  | Texp_while (e1, e2) ->
    let acc = process_expression ~exp:e1 init in
    process_expression ~exp:e2 acc
  | Texp_for (ident, _, e1, e2, _, e3) ->
    let idents_to_types =
      String.Map.add (Ident.unique_name ident) (e1.exp_type, e1.exp_env)
        idents_to_types
    in
    let acc = process_expression ~exp:e1 (idents_to_types, app_points) in
    let acc = process_expression ~exp:e2 acc in
    process_expression ~exp:e3 acc
  | Texp_lazy exp
  | Texp_assert exp
  | Texp_field (exp, _, _) ->
    process_expression ~exp init
  | Texp_setfield (e1, _, _, e2) ->
    let acc = process_expression ~exp:e1 init in
    process_expression ~exp:e2 acc
  | Texp_send (exp, _meth, e_opt) ->
    (* TODO: handle methods *)
    let acc = process_expression ~exp init in
    begin match e_opt with
    | None -> acc
    | Some exp -> process_expression ~exp acc
    end
  | Texp_letmodule (_ident, _str_loc, _mod_expr, exp) ->
    (* TODO: handle [mod_expr] *)
(*
    let idents_to_types =
      String.Map.add (Ident.unique_name ident)
        (mod_expr.mod_type, mod_expr.mod_env) idents_to_types
    in
*)
    process_expression ~exp init
  (* CR mshinwell: this needs finishing, yuck *)
  | Texp_ident _
  | Texp_constant _
  | Texp_variant _
  | Texp_new _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_object _
  | Texp_pack _
  | Texp_unreachable
  | Texp_letexception _
  | Texp_extension_constructor _ -> idents_to_types, app_points

and process_cases ~cases init =
  List.fold_left cases ~init ~f:(fun acc case ->
    let pat = case.T.c_lhs in
    let exp = case.T.c_rhs in
    let idents_to_types, app_points = process_expression ~exp acc in
    process_pattern ~pat ~idents_to_types, app_points
  )

and process_value_binding ~value_binding init =
  List.fold_left value_binding ~init ~f:(fun acc value_binding ->
    let pat = value_binding.T.vb_pat in
    let exp = value_binding.T.vb_expr in
    let idents_to_types, app_points = process_expression ~exp acc in
    process_pattern ~pat ~idents_to_types, app_points
  )

let rec process_module_expr ~(mod_expr : T.module_expr)
      ((idents_to_types, app_points) as maps) =
  match mod_expr.mod_desc with
  | Tmod_ident _ -> maps
  | Tmod_structure structure ->
    process_implementation ~structure ~idents_to_types ~app_points
  | Tmod_constraint (mod_expr, _, _, _)
  | Tmod_functor (_, _, _, mod_expr) ->
    process_module_expr ~mod_expr maps
  | Tmod_apply (me1, me2, _) ->
    let maps = process_module_expr ~mod_expr:me1 maps in
    process_module_expr ~mod_expr:me2 maps
  | Tmod_unpack (_expr, _) -> (* TODO *) maps

and process_implementation ~(structure : T.structure)
      ~idents_to_types ~app_points =
  List.fold_left structure.str_items
    ~init:(idents_to_types, app_points)
    ~f:(fun maps (str_item : T.structure_item) ->
          match str_item.str_desc with
          | Tstr_value (_rec, value_binding) ->
            process_value_binding ~value_binding maps
          | Tstr_eval (exp, _) ->
            process_expression ~exp maps
          | Tstr_module module_binding ->
            process_module_expr ~mod_expr:module_binding.mb_expr maps
          | Tstr_recmodule lst ->
            List.fold_left lst ~init:(idents_to_types, app_points) ~f:(
              fun maps (module_binding : T.module_binding) ->
                process_module_expr ~mod_expr:module_binding.mb_expr maps
            )
          | Tstr_primitive _
          | Tstr_type _
          | Tstr_exception _
          | Tstr_modtype _
          | Tstr_open _
          | Tstr_class _
          | Tstr_class_type _
          | Tstr_include _
          | Tstr_attribute _
          | Tstr_typext _ -> maps)

let create_idents_to_types_map ~(cmt_infos : Cmt_format.cmt_infos) =
  let cmt_annots = cmt_infos.cmt_annots in
  match cmt_annots with
  | Packed _
  | Interface _
  (* CR mshinwell: find out what "partial" implementations and
      interfaces are, and fix cmt_format.mli so it tells you *)
  | Partial_implementation _
  | Partial_interface _ -> String.Map.empty, LocTable.empty
  | Implementation structure ->
    process_implementation ~structure ~idents_to_types:String.Map.empty
      ~app_points:LocTable.empty

let load_path_from_cmt_infos (cmt_infos : Cmt_format.cmt_infos) =
  List.map cmt_infos.cmt_loadpath ~f:(fun leaf ->
    if Filename.is_relative leaf then
      Filename.concat cmt_infos.Cmt_format.cmt_builddir leaf
    else leaf)

let load_from_channel_then_close ~filename chan ~add_to_load_path =
  if debug then Printf.printf "attempting to load cmt file: %s\n%!" filename;
  let cmt_infos = Cmt_format.read_cmt_from_channel ~filename chan in
  add_to_load_path (load_path_from_cmt_infos cmt_infos);
  let idents_to_types, _application_points =
    let idents, app_points = create_idents_to_types_map ~cmt_infos in
    try
      let idents =
        String.Map.map (fun (type_expr, env) ->
          type_expr, Env.env_of_only_summary Envaux.env_from_summary env
        ) idents
      in
      let app_points =
        LocTable.map (fun (type_expr, env) ->
          type_expr, Env.env_of_only_summary Envaux.env_from_summary env
        ) app_points
      in
      let distinguished_ident =
        let ident = ref None in
        String.Map.iter
          (fun candidate type_expr_and_env -> 
            try
              if String.sub candidate 0 (String.length distinguished_var_name)
                = distinguished_var_name
              then begin
                ident := Some (candidate, type_expr_and_env)
              end
            with _exn -> ())
          idents;
        !ident
      in
      let idents =
        (* CR mshinwell: work out a proper solution to this problem *)
        match distinguished_ident with
        | None -> idents
        | Some (_ident, type_expr_and_env) ->
          String.Map.add distinguished_var_name type_expr_and_env idents
      in
      idents, app_points
    with
    | Envaux.Error (Envaux.Module_not_found path) ->
      begin if debug then begin
        Printf.printf "cmt load failed: module '%s' missing\n%!"
          (Path.name path)
      end;
      String.Map.empty, LocTable.empty
      end
    | exn -> begin
      if debug then
        Printf.printf "exception whilst reading cmt file(s): %s\n%!"
          (Printexc.to_string exn);
      String.Map.empty, LocTable.empty
    end
  in
  let t =
    { cmt_infos;
      idents_to_types;
    }
  in
  Some t

let type_of_ident t ~name ~stamp =
  let unique_name = Printf.sprintf "%s_%d" name stamp in
  try Some (String.Map.find unique_name t.idents_to_types)
  with Not_found -> begin
    if debug then Printf.printf "type_of_ident failed\n%!";
    None
  end

let cmt_infos t = t.cmt_infos
