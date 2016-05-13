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

module List = ListLabels
module Variant_kind = Type_oracle.Variant_kind

let debug = Monda_debug.debug

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
    | Indexed of { index : int; next : parsed; }
    | Record of { field_name : string; next : parsed; }

  let rec parse ~path =
    let split = Misc.Stdlib.String.split path ~on:'.' in
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
            Some (Record { field_name = component; next; }))
      split
      (Some Identity)

  let find_value t ~path ~type_expr_and_env ~must_be_mutable v =
    match type_expr_and_env with
    | None -> None
    | Some (type_expr, env) ->
      let path = parse ~path in
      let rec find_component ~path ~type_expr ~env ~previous_was_mutable v =
        let oracle_result =
          Our_type_oracle.find_type_information t.type_oracle ~formatter
            ~type_expr_and_env:(Some (type_expr, env)) ~scrutinee:v
        in
        match path with
        | Identity ->
          if must_be_mutable && (not previous_was_mutable) then
            None
          else
            Some v
        | Record { field_name; next; } ->
          begin match oracle_result with
          | Record (_path, params, args, fields, _record_repr, env) ->
            let fields = Array.of_list fields in
            let found = ref None in
            for index = 0 to Array.length fields - 1 do
              let decl = fields.(index) in
              if Ident.name decl.ld_id = desired_field then begin
                found := Some (index, decl)
              end
            done;
            begin match found with
            | None -> None
            | Some (index, decl) ->
              if (not (D.is_block v))
                || D.Obj.size_exn v <= index
              then
                None
              else
                let field = D.Obj.field_exn v index in
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
                  ~previous_was_mutable:field_is_mutable field
            end
          | _ -> None
          end
        | Indexed { index; next; } ->
          begin match oracle_result with
          | Array (element_type, env) ->
            if index >= D.Obj.size_exn v then
              None
            else
              let element = D.Obj.field_exn v index in
              find_component ~path:next ~type_expr:element_type ~env
                ~previous_was_mutable:false element
          | Tuple (element_types, env) ->
            let element_types = Array.of_list element_types in
            if index >= D.Obj.size_exn v
              || index >= Array.length element_types
            then
              None
            else
              let element = D.Obj.field_exn v index in
              let element_type = element_types.(index) in
              find_component ~path:next ~type_expr:element_type ~env
                ~previous_was_mutable:false element
          | _ -> None
          end
      in
      find_component ~path ~type_expr v
end
