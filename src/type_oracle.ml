(**************************************************************************)
(*                                                                        *)
(*                Make OCaml native debugging awesome!                    *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(* Copyright (c) 2013--2019 Jane Street Group, LLC                        *)
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
let print_path = Monda_debug.print_path

module List = ListLabels

module Variant_kind = struct
  type t = Polymorphic | Non_polymorphic

  let to_string_prefix = function
    | Polymorphic -> "`"
    | Non_polymorphic -> ""
end
module Vk = Variant_kind

module Result = struct
  type t =
    | Obj_boxed_traversable
    | Obj_boxed_not_traversable
    (* CR mshinwell: naming *)
    | Obj_immediate
    | Obj_immediate_but_should_be_boxed
    | Unit
    | Abstract of Path.t
    | Array of Types.type_expr * Env.t
    | List of Types.type_expr * Env.t
    | Tuple of Types.type_expr list * Env.t
    | Char
    | Int
    | Float
    | Float_array
    | Constant_constructor of string * Variant_kind.t
    | Non_constant_constructor of Path.t * Types.constructor_declaration list
        * Types.type_expr list * Types.type_expr list * Env.t
        * Variant_kind.t
    | Record of Path.t * Types.type_expr list * Types.type_expr list
        * Types.label_declaration list * Types.record_representation
        * Env.t
    | Open
    | Ref of Types.type_expr * Env.t
    | String
    | Closure
    | Lazy
    | Object
    | Abstract_tag
    | Format6
    | Stdlib_set of { env : Env.t; element_ty : Types.type_expr; }
    | Stdlib_map of {
        key_env : Env.t;
        key_ty : Types.type_expr;
        datum_env : Env.t;
        datum_ty : Types.type_expr;
      }
    | Stdlib_hashtbl of {
        key_env : Env.t;
        key_ty : Types.type_expr;
        datum_env : Env.t;
        datum_ty : Types.type_expr;
      }
    | Custom
    | Module of Types.module_type
    | Unknown

  let to_string = function
    | Obj_boxed_traversable -> "Obj_boxed_traversable"
    | Obj_boxed_not_traversable -> "Obj_boxed_not_traversable"
    | Obj_immediate -> "Obj_immediate"
    | Obj_immediate_but_should_be_boxed -> "Obj_immediate_but_should_be_boxed"
    | Unit -> "Unit"
    | Abstract _ -> "Abstract"
    | Array _ -> "Array"
    | List _ -> "List"
    | Tuple _ -> "Tuple"
    | Char -> "Char"
    | Int -> "Int"
    | Float -> "Float"
    | Float_array -> "Float_array"
    | Constant_constructor _ -> "Constant_constructor"
    | Non_constant_constructor _ -> "Non_constant_constructor"
    | Record _ -> "Record"
    | Open -> "Open"
    | Ref _ -> "Ref"
    | String -> "String"
    | Closure -> "Closure"
    | Lazy -> "Lazy"
    | Object -> "Object"
    | Abstract_tag -> "Abstract_tag"
    | Format6 -> "Format6"
    | Stdlib_set _ -> "Stdlib.Set"
    | Stdlib_map _ -> "Stdlib.Map"
    | Stdlib_hashtbl _ -> "Stdlib.Hashtbl"
    | Custom -> "Custom"
    | Module _ -> "Module"
    | Unknown -> "Unknown"
end

module Make (D : Debugger.S) (Cmt_cache : Cmt_cache_intf.S) = struct
  module V = D.Value

  module Abstraction_breaker = Abstraction_breaker.Make (D) (Cmt_cache)

  type maybe_boxed = Unboxed of V.t | Boxed of V.t | Absent

  type t = {
    abstraction_breaker : Abstraction_breaker.t;
    cmt_cache : Cmt_cache.t;
  }

  let create ~cmt_cache =
    { abstraction_breaker = Abstraction_breaker.create ~cmt_cache;
      cmt_cache;
    }

  let extract_constant_ctors ~cases =
    let constant_ctors, _ =
      List.fold_left cases
        ~init:([], 0)
        ~f:(fun (constant_ctors, next_ctor_number) ctor_decl ->
              let ident = ctor_decl.Types.cd_id in
              match ctor_decl.Types.cd_args with
              | Cstr_tuple [] ->
                (next_ctor_number, ident)::constant_ctors, next_ctor_number + 1
              | Cstr_tuple _ | Cstr_record _ ->
                constant_ctors, next_ctor_number)
    in
    constant_ctors

  let format6_path =
    Path.(Pdot (Pident (Ident.create_persistent "CamlinternalFormatBasics"),
      "format6", 0))

  let check_predef_paths ~path ~args ~env ~scrutinee : Result.t option =
    (* CR mshinwell: validate more things (e.g. value size) *)
    if Path.same path Predef.path_array then
      match scrutinee with
      | Unboxed _ -> Some Obj_immediate_but_should_be_boxed
      | Absent | Boxed _ ->
        match args with
        | [arg] -> Some (Array (arg, env))
        | _ -> Some Obj_boxed_traversable  (* wrong number of arguments *)
    else if Path.same path Predef.path_list then
      match args with
      | [arg] -> Some (List (arg, env))
      | _ ->
        match scrutinee with  (* wrong number of arguments *)
        | Unboxed _ -> Some Obj_immediate
        | Boxed _ -> Some Obj_boxed_traversable
        | Absent -> Some Unknown
    else if Path.same path Predef.path_int then
      match scrutinee with
      | Unboxed _ | Absent -> Some Int
      | Boxed _ -> Some Obj_boxed_traversable  (* should not be boxed *)
    else if Path.same path Predef.path_char then
      match scrutinee with
      | Unboxed _ | Absent -> Some Char
      | Boxed _ -> Some Obj_boxed_traversable  (* should not be boxed *)
    else if Path.same path Predef.path_unit then
      match scrutinee with
      | Unboxed _ | Absent -> Some Unit
      | Boxed _ -> Some Obj_boxed_traversable  (* should not be boxed *)
    else if Path.same path format6_path then
      match scrutinee with
      | Unboxed _ -> Some Obj_immediate
      | Boxed _ -> Some Format6
      | Absent -> Some Unknown
    else
      None

  (* CR-someday mshinwell: Replace this with proper custom printer support. *)
  let check_special_cases _t env (path : Path.t) ~args : Result.t option =
    match path with
    | Pdot (Papply (Pdot (Pident mod_name, functor_name, _), applied_to),
        "t", _) ->
      begin match Ident.name mod_name, functor_name with
      | "Stdlib__set", "Make" ->
        begin match args with
        | [] ->
          let element_ty_path : Path.t =
            (* [applied_to] should satisfy [Set.OrderedType]. *)
            Pdot (applied_to, "t", 0)
          in
          let type_desc : Types.type_desc =
            Tconstr (element_ty_path, [], ref Types.Mnil)
          in
          let element_ty = Btype.newgenty type_desc in
          Some (Stdlib_set { env; element_ty; })
        | _ -> None
        end
      | "Stdlib__map", "Make" ->
        begin match args with
        | [datum_ty] ->
          let key_ty_path : Path.t =
            (* [applied_to] should satisfy [Map.OrderedType]. *)
            Pdot (applied_to, "t", 0)
          in
          let type_desc : Types.type_desc =
            Tconstr (key_ty_path, [], ref Types.Mnil)
          in
          let key_ty = Btype.newgenty type_desc in
          Some (Stdlib_map {
            key_env = env;
            key_ty;
            datum_env = env;
            datum_ty;
          })
        | _ -> None
        end
      | "Stdlib__hashtbl", "Make" ->
        begin match args with
        | [datum_ty] ->
          let key_ty_path : Path.t =
            (* [applied_to] should satisfy [Hashtbl.HashedType]. *)
            Pdot (applied_to, "t", 0)
          in
          let type_desc : Types.type_desc =
            Tconstr (key_ty_path, [], ref Types.Mnil)
          in
          let key_ty = Btype.newgenty type_desc in
          Some (Stdlib_hashtbl {
            key_env = env;
            key_ty;
            datum_env = env;
            datum_ty;
          })
        | _ -> None
        end
      | _, _ -> None
      end
    | _ -> None

  let rec examine_type_expr t ~formatter ~paths_visited_so_far ~type_expr ~env
        ~scrutinee : Result.t =
    let type_expr = Ctype.expand_head env type_expr in
(*Format.fprintf formatter "type_expr: %a\n%!" Printtyp.raw_type_expr type_expr;*)
    match type_expr.Types.desc with
    | Types.Tconstr (path, args, _abbrev_memo_ref) ->
      begin match check_predef_paths ~path ~args ~env ~scrutinee with
      | Some result -> result
      | None ->
        if debug then
           Printf.printf "examine_type_expr: Env.find_type '%s': "
             (print_path path);
        begin match try Some (Env.find_type path env) with Not_found -> None with
        | None ->
          if debug then Printf.printf "not found.\n%!";
          (* Even if the type is abstract, the declaration should still be in
             the environment. *)
          begin match scrutinee with
          | Absent -> Unknown
          | Unboxed _ -> Obj_immediate
          | Boxed _ ->
            if debug then
              Printf.printf "examine_type_expr error case 1, load path is: %s\n%!"
                (String.concat "," !Config.load_path);
            Obj_boxed_traversable
          end
        | Some type_decl ->
          if debug then Printf.printf "found.\n%!";
          examine_type_decl t ~formatter ~paths_visited_so_far ~type_expr ~env
            ~path ~args ~type_decl ~scrutinee
        end
      end
    | Types.Tvariant row_desc ->
      begin match scrutinee with
      | Absent -> Unknown
      | Boxed _ ->
        (* CR mshinwell: support boxed polymorphic variant constructors *)
        if debug then Printf.printf "examine_type_expr error case 2\n%!";
        Obj_boxed_traversable
      | Unboxed scrutinee ->
        let ctor_names_and_hashes =
          let labels = List.map row_desc.Types.row_fields ~f:fst in
          List.map labels ~f:(fun label -> label, Btype.hash_variant label)
        in
        match V.int scrutinee with
        | None -> Obj_immediate  (* CR mshinwell: error? *)
        | Some desired_hash ->
          let matches =
            List.filter ctor_names_and_hashes
              ~f:(fun (_ctor_name, hash) -> hash = desired_hash)
          in
          begin match matches with
          | [(ctor_name, _hash)] ->
            Constant_constructor (ctor_name, Vk.Polymorphic)
          | _::_ | [] -> Obj_immediate  (* cannot find ctor with given hash *)
          end
      end
    | Types.Ttuple component_types ->
      begin match scrutinee with
      | Absent | Boxed _ -> Tuple (component_types, env)
      | Unboxed _ -> Obj_immediate_but_should_be_boxed
      end
    | Types.Tarrow _ ->
      begin match scrutinee with
      | Absent | Boxed _ -> Closure
      | Unboxed _ -> Obj_immediate_but_should_be_boxed
      end
    | Types.Tvar _
    | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tsubst _
    | Types.Tunivar _ | Types.Tpoly _ | Types.Tpackage _ ->
      (* CR mshinwell: more work to do here *)
      begin match scrutinee with
      | Absent | Boxed _ ->
        let what =
          match (Btype.repr type_expr).Types.desc with
          | Types.Tvar _ -> "Tvar"
          | Types.Tobject _ -> "Tobject"
          | Types.Tfield _ -> "Tfield"
          | Types.Tnil -> "Tnil"
          | Types.Tsubst _ -> "Tsubst"
          | Types.Tunivar _ -> "Tunivar"
          | Types.Tpoly _ -> "Tpoly"
          | Types.Tpackage _ -> "Tpackage"
          | _ -> assert false
        in
        if debug then Printf.printf "examine_type_expr error case 3 %s\n%!" what;
        Obj_boxed_traversable
      | Unboxed _ -> Obj_immediate
      end
    | Types.Tlink _type_expr ->
      (* Should have been eliminated by [Btype.repr]. *)
      assert false

  (* CR mshinwell: Should we use [Ctype.extract_concrete_typedecl]? *)
  and examine_type_decl t ~formatter ~paths_visited_so_far ~type_expr ~env
        ~path ~args ~type_decl ~scrutinee : Result.t =
    let params = type_decl.Types.type_params in
    if List.length params <> List.length args then begin
      if debug then Printf.printf "type params/args don't match\n%!";
      Abstract path  (* fail gracefully *)
    end else begin
      match type_decl.Types.type_manifest with
      | Some type_expr ->
        examine_type_expr t ~formatter ~paths_visited_so_far ~type_expr ~env
          ~scrutinee
      | None ->
        begin match type_decl.Types.type_kind with
        | Types.Type_variant cases ->
          begin match scrutinee with
          | Absent -> Unknown  (* CR mshinwell: improve *)
          | Boxed _ ->
            (* CR mshinwell: change this when Tvariant case is filled in above *)
            let kind = Vk.Non_polymorphic in
            Non_constant_constructor (path, cases, params, args, env, kind)
          | Unboxed scrutinee ->
            let constant_ctors = extract_constant_ctors ~cases in
            match V.int scrutinee with
            | None -> Obj_immediate (* CR mshinwell: as above *)
            | Some value ->
              if value >= 0 && value < List.length constant_ctors then
                let ident =
                  try Some (List.assoc value constant_ctors)
                  with Not_found -> None
                in
                begin match ident with
                | Some ident ->
                  Constant_constructor (Ident.name ident, Vk.Non_polymorphic)
                | None -> Obj_immediate
                end
              else
                Obj_immediate
          end
        | Types.Type_abstract ->
          if List.mem path ~set:paths_visited_so_far then begin
            if debug then begin
              Printf.printf "loop resolving %s\n%!" (print_path path)
            end;
            Abstract path  (* fail gracefully *)
          end else begin
            let paths_visited_so_far = path::paths_visited_so_far in
            discover_manifest t ~formatter ~paths_visited_so_far ~type_expr
              ~path ~args ~env ~scrutinee
          end
        | Types.Type_record (field_decls, record_repr) ->
          begin match scrutinee with
          | Absent | Boxed _ ->
            if scrutinee <> Absent
              && List.length field_decls = 1
              && List.length args = 1
              && Ident.name ((List.hd field_decls).Types.ld_id) = "contents"
              && Path.name path = "Pervasives.ref"
            then
              Ref (List.hd args, env)
            else
              Record (path, params, args, field_decls, record_repr, env)
          | Unboxed _ ->
            (* Records should never be unboxed values, but behave gracefully. *)
            Obj_immediate
          end
        | Types.Type_open -> Open
        end
    end

  (* CR-soon mshinwell: try removing [type_expr], probably redundant *)
  and discover_manifest t ~formatter ~paths_visited_so_far ~type_expr ~path
        ~args ~env ~scrutinee : Result.t =
    match check_special_cases t env path ~args with
    | Some result -> result
    | None ->
      let manifest =
        Abstraction_breaker.find_manifest_of_abstract_type t.abstraction_breaker
          ~formatter ~path ~env
      in
      match manifest with
      | None -> Abstract path  (* couldn't find manifest; fail gracefully *)
      | Some (path, type_decl, env) ->
        examine_type_decl t ~formatter ~paths_visited_so_far ~type_expr ~env
          ~path ~args ~type_decl ~scrutinee

  let find_type_information t ~formatter ~type_expr_and_env ~scrutinee =
    if debug then begin
      Format.printf "find_type_information starting (scrutinee %a), \
          type info present? %s\n%!"
        V.print scrutinee
        (match type_expr_and_env with None -> "no" | Some _ -> "yes")
    end;
    let absent = V.is_null scrutinee in
    let result : Result.t =
      try
        if absent || V.is_int scrutinee then
          match type_expr_and_env with
          | None -> if absent then Unknown else Obj_immediate
          | Some (type_expr, env) ->
            examine_type_expr t ~formatter ~paths_visited_so_far:[] ~type_expr
              ~env ~scrutinee:(Unboxed scrutinee)
        else
          (* CR mshinwell: this is an example of a place we may get a
            Read_error.  Perhaps V.tag_exn and so on should return options.
          *)
          let tag = V.tag_exn scrutinee in
          match tag with
          | tag when tag < Obj.lazy_tag ->
            begin match type_expr_and_env with
            | None -> Obj_boxed_traversable
            | Some (type_expr, env) ->
              examine_type_expr t ~formatter ~paths_visited_so_far:[] ~type_expr
                ~env ~scrutinee:(Boxed scrutinee)
            end
          | tag when tag = Obj.string_tag -> String
          | tag when tag = Obj.double_tag -> Float
          | tag when (tag = Obj.closure_tag || tag = Obj.infix_tag) -> Closure
          | tag when tag = Obj.lazy_tag -> Lazy
          | tag when tag = Obj.object_tag -> Object
          | tag when tag = Obj.forward_tag -> Obj_boxed_traversable
          | tag when tag = Obj.abstract_tag -> Abstract_tag
          | tag when tag = Obj.custom_tag -> Custom
          | tag when tag = Obj.double_array_tag -> Float_array
          | tag when tag < Obj.no_scan_tag -> Obj_boxed_traversable
          | _tag -> Obj_boxed_not_traversable
      (* CR mshinwell: Fix code properly so this handler isn't needed *)
      with _ -> Obj_immediate
    in
    if debug then begin
      Printf.printf "find_type_information returning %s\n%!"
        (Result.to_string result)
    end;
    result

  let find_type_information t ~formatter
        (type_and_env : (Cmt_file.core_or_module_type * Env.t) option)
        ~scrutinee =
    match type_and_env with
    | None ->
      find_type_information t ~formatter ~type_expr_and_env:None ~scrutinee
    | Some (Core type_expr, env) ->
      let type_expr_and_env = Some (type_expr, env) in
      find_type_information t ~formatter ~type_expr_and_env ~scrutinee
    | Some (Module mod_type, _env) ->
      if V.is_null scrutinee then Unknown
      else if V.is_int scrutinee then Obj_immediate
      else
        let tag = V.tag_exn scrutinee in
        if tag = 0 then Module mod_type
        else if tag < Obj.lazy_tag then Obj_boxed_traversable
        else Obj_boxed_not_traversable
end
