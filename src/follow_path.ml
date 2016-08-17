(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2016 Jane Street Group, LLC                              *)
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

module Make (D : Debugger.S) = struct
  module Our_type_oracle = Type_oracle.Make (D)

  type t = {
    type_oracle : Our_type_oracle.t;
    cmt_cache : Cmt_cache.t;
  }

  let create ~cmt_cache =
    { type_oracle = Our_type_oracle.create ~cmt_cache;
      cmt_cache;
    }

  (* CR-someday mshinwell: extend to support other things *)
  type parsed =
    | Identity
    | Module of { name : string; next : parsed; }
    | Indexed of { index : int; next : parsed; }
    | Record of { field_name : string; next : parsed; }

  type toplevel_parsed =
    | Module of { name : string; next : parsed; }
    | Variable of { name : string; next : parsed; }

  let starts_with_capital str =
    let first_letter = String.get str 0 in
    let upper_first_letter = Char.uppercase_ascii first_letter in
    first_letter = upper_first_letter

  let parse ~path : toplevel_parsed option =
    let split = String.split_on_char '.' path in
    match split with
    | [] -> None
    | [name] ->
      if starts_with_capital name then None
      else Some (Variable { name; next = Identity; })
    | name::rest_of_path ->
      let next =
        List.fold_right (fun component next ->
            match next with
            | None -> None
            | Some next ->
              if String.length component >= 3
                && String.get component 0 = '('
                && String.get component (String.length component - 1) = ')'
              then
                let index =
                  String.sub component 1 (String.length component - 2)
                in
                match int_of_string index with
                | exception _ -> None
                | index ->
                  if index < 0 then None
                  else Some (Indexed { index; next; })
              else
                let first_letter = String.get component 0 in
                let upper_first_letter = Char.uppercase_ascii first_letter in
                if first_letter = upper_first_letter then
                  Some (Module { name = component; next; })
                else
                  Some (Record { field_name = component; next; }))
          rest_of_path
          (Some Identity)
      in
      match next with
      | None -> None
      | Some next ->
        if starts_with_capital name then Some (Module { name; next; })
        else Some (Variable { name; next; })

  let path_looks_ok ~path =
    match parse ~path with
    | None -> false
    | Some _ -> true

  let strip_module_component_prefix (path : toplevel_parsed) =
    match path with
    | Variable _ -> None
    | Module { name; next; } ->
      let rec strip (path : parsed) ~names =
        match path with
        | Module { name; next; } ->
          strip next ~names:(name::names)
        | Record { field_name; next; } ->
          (* This will actually be the "foo" in "M.foo" or "M.N.foo". *)
          Some (List.rev names, field_name, next)
        | Identity
        | Indexed _ -> None
      in
      strip next ~names:[name]

  type _ lvalue_or_rvalue =
    | Lvalue : D.target_addr lvalue_or_rvalue
    | Rvalue : (D.Obj.t * Types.type_expr * Env.t) lvalue_or_rvalue

  let evaluate_given_starting_point (type obj_or_addr) t ~path
        ~type_expr_and_env ~(lvalue_or_rvalue : obj_or_addr lvalue_or_rvalue)
        ~must_be_mutable ~formatter v : obj_or_addr option =
    match type_expr_and_env with
    | None -> None
    | Some (type_expr, env) ->
      let rec find_component ~path ~type_expr ~env ~previous_was_mutable
            ~(address_of_v : D.target_addr option) (v : D.Obj.t)
            : obj_or_addr option =
        let oracle_result =
          Our_type_oracle.find_type_information t.type_oracle ~formatter
            ~type_expr_and_env:(Some (type_expr, env)) ~scrutinee:v
        in
        match path with
        | Identity ->
          if must_be_mutable && (not previous_was_mutable) then begin
            None
          end else begin
            match lvalue_or_rvalue with
            | Lvalue -> address_of_v
            | Rvalue -> Some (v, type_expr, env)
          end
        | Module _ ->
          (* CR-soon mshinwell: fill this in for toplevel accesses, e.g.
             M.constant *)
          None
        | Record { field_name; next; } ->
          begin match oracle_result with
          | Record (_path, params, args, fields, _record_repr, env) ->
            let fields = Array.of_list fields in
            let found = ref None in
            for index = 0 to Array.length fields - 1 do
              let decl = fields.(index) in
              if Ident.name decl.ld_id = field_name then begin
                found := Some (index, decl)
              end
            done;
            begin match !found with
            | None -> None
            | Some (index, decl) ->
              if (not (D.Obj.is_block v))
                || D.Obj.size_exn v <= index
              then
                None
              else
                let field = D.Obj.field_exn v index in
                let address_of_field = D.Obj.address_of_field v index in
                let field_type =
                  try Ctype.apply env params decl.ld_type args
                  with Ctype.Cannot_apply -> decl.ld_type
                in
                let field_is_mutable =
                  match decl.ld_mutable with
                  | Immutable -> false
                  | Mutable -> true
                in
                find_component ~path:next ~type_expr:field_type ~env
                  ~previous_was_mutable:field_is_mutable
                  ~address_of_v:(Some address_of_field)
                  field
            end
          | _ -> None
          end
        | Indexed { index; next; } ->
          begin match oracle_result with
          | Array (element_type, env) ->
            if index >= D.Obj.size_exn v then
              None
            else
              let address_of_element = D.Obj.address_of_field v index in
              let element = D.Obj.field_exn v index in
              find_component ~path:next ~type_expr:element_type ~env
                ~previous_was_mutable:false
                ~address_of_v:(Some address_of_element)
                element
          | Tuple (element_types, env) ->
            let element_types = Array.of_list element_types in
            if index >= D.Obj.size_exn v
              || index >= Array.length element_types
            then
              None
            else
              let address_of_element = D.Obj.address_of_field v index in
              let element = D.Obj.field_exn v index in
              let element_type = element_types.(index) in
              find_component ~path:next ~type_expr:element_type ~env
                ~previous_was_mutable:false
                ~address_of_v:(Some address_of_element)
                element
          | _ -> None
          end
      in
      find_component ~path ~type_expr ~previous_was_mutable:false
        ~address_of_v:None ~env v

  let evaluate t ~path ~lvalue_or_rvalue ~must_be_mutable
        ~cmt_file_search_path ~formatter =
    let found ~starting_point ~dwarf_type ~rest_of_path =
      let type_expr_and_env =
        Type_helper.type_expr_and_env_from_dwarf_type ~dwarf_type
          ~cmt_cache:t.cmt_cache ~cmt_file_search_path
      in
      evaluate_given_starting_point t ~path:rest_of_path
        ~type_expr_and_env ~lvalue_or_rvalue ~must_be_mutable
        ~formatter starting_point
    in
    match parse ~path with
    | None -> None
    | Some path ->
      match strip_module_component_prefix path with
      | None ->
        begin match path with
        | Module _ -> None
        | Variable { name; next = rest_of_path; } ->
          match D.find_named_value ~name with
          | Not_found -> None
          | Found (starting_point, dwarf_type) ->
            found ~starting_point ~dwarf_type ~rest_of_path
        end
      | Some (module_names, module_component, rest_of_path) ->
        let module_component_path =
          String.concat "." (module_names @ [module_component])
        in
        (* CR mshinwell: Think again about the name of find_global_symbol *)
        (* Note that [find_global_symbol] always returns an rvalue, even in
           the case of inconstant ("Initialize_symbol") bindings.  (See
           comments in asmcomp/debug/dwarf.ml in the compiler.) *)
        match D.find_global_symbol ~name:module_component_path with
        | Not_found -> None
        | Found (starting_point, dwarf_type) ->
(*
Format.eprintf "starting_point %a type %s\n%!" D.Obj.print starting_point
  dwarf_type;
*)
          found ~starting_point ~dwarf_type ~rest_of_path
end
