(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2016--2019 Jane Street Group, LLC                        *)
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

let debug = Monda_debug.debug

module Make (D : Debugger.S) (Cmt_cache : Cmt_cache_intf.S) = struct
  module Our_type_oracle = Type_oracle.Make (D) (Cmt_cache)
  module Type_helper = Type_helper.Make (D) (Cmt_cache)

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
    | Project_index of { index : int; next : parsed; }
    | Project_name of { field_name : string; next : parsed; }

  type toplevel_parsed =
    | Module of { name : string; next : parsed; }
    | Variable of { name : string; next : parsed; }

  type what_was_above = Module | Core_value

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
                  else Some (Project_index { index; next; })
              else
                let first_letter = String.get component 0 in
                let upper_first_letter = Char.uppercase_ascii first_letter in
                if first_letter = upper_first_letter then
                  Some (Module { name = component; next; })
                else
                  Some (Project_name { field_name = component; next; }))
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

  type _ lvalue_or_rvalue =
    | Lvalue : D.target_addr lvalue_or_rvalue
    | Rvalue : (D.Obj.t * Cmt_file.core_or_module_type * Env.t) lvalue_or_rvalue

  let evaluate_given_starting_point (type obj_or_addr) t ~path
        type_and_env ~(lvalue_or_rvalue : obj_or_addr lvalue_or_rvalue)
        ~must_be_mutable ~formatter ~(what_was_above : what_was_above)
        v : obj_or_addr option =
    match type_and_env with
    | None -> None
    | Some (ty, env, _is_parameter) ->
      let rec find_component ~path ~ty ~env ~previous_was_mutable
            ~(address_of_v : D.target_addr option)
            ~(what_was_above : what_was_above)
            (v : D.Obj.t)  (* CR mshinwell: this should be a [Value.t] *)
            : obj_or_addr option =
        let oracle_result =
          let scrutinee = D.Value.create_exists_on_target v in
          Our_type_oracle.find_type_information t.type_oracle ~formatter
            (Some (ty, env)) ~scrutinee
        in
        match path with
        | Identity ->
          if must_be_mutable && (not previous_was_mutable) then begin
            None
          end else begin
            match lvalue_or_rvalue with
            | Lvalue -> address_of_v
            | Rvalue -> Some (v, ty, env)
          end
        | Module _ ->
          begin match what_was_above with
          | Core_value ->
            Format.fprintf formatter "@{<error_colour>Error: @}Cannot \
              project a module out of a core (non-module) value\n%!";
            None
          | Module ->
            (* Projecting modules from modules is going to be similar to
               the values from modules case below. *)
            None
          end
        | Project_name { field_name; next; } ->
          begin match what_was_above with
          | Module ->
            begin match ty with
            | Core _ ->
              (* CR mshinwell: Improve error messages *)
              Format.fprintf formatter "@{<error_colour>Error: @}Expected \
                  module type for projecting @{<variable_name_colour>%s@}, \
                  but have core type\n%!"
                field_name;
              None
            | Module mod_ty ->
              let cannot_handle what =
                Format.fprintf formatter "@{<error_colour>Error: @}Cannot \
                    project @{<variable_name_colour>%s@} from a module \
                    (unsupported %s case)\n%!"
                  field_name
                  what;
                None
              in
              match mod_ty with
              | Mty_ident path ->
                cannot_handle (
                  Printf.sprintf "Mty_ident (%s)" (Path.name path));
              | Mty_signature sig_items ->
                let _pos, found_val_desc =
                  List.fold_left
                    (fun (pos, found_val_desc) (item : Types.signature_item) ->
                      match found_val_desc with
                      | Some _ -> pos, found_val_desc
                      | None ->
                        match item with
                        | Sig_value (ident, val_desc) ->
                          if String.equal field_name (Ident.name ident) then
                            pos (* doesn't matter *), Some (pos, val_desc)
                          else
                            let pos =
                              match val_desc.val_kind with
                              | Val_prim _ -> pos
                              | _ -> pos + 1
                            in
                            pos, None
                        | Sig_typext _
                        | Sig_module _ (* XXX need to calculate its length *)
                        | Sig_class _ -> pos + 1, None
                        | Sig_type _
                        | Sig_modtype _
                        | Sig_class_type _ -> pos, None)
                    (0, None)
                    sig_items
                in
                begin match found_val_desc with
                | None ->
                  Format.fprintf formatter "@{<error_colour>Error: @}Cannot \
                      project non-existent field @{<variable_name_colour>%s@} \
                      from module\n%!"
                    field_name;
                  None
                | Some (pos, val_desc) ->
                  if not (D.Obj.is_block v)
                    || D.Obj.tag_exn v <> 0
                    || D.Obj.size_exn v <= pos
                  then begin
                    Format.fprintf formatter "@{<error_colour>Error: @}Cannot \
                        project field @{<variable_name_colour>%s@} \
                        from module: block %a is malformed\n%!"
                      field_name
                      D.Obj.print v;
                    None
                  end else begin
                    let ty : Cmt_file.core_or_module_type =
                      Core val_desc.val_type
                    in
                    let address_of_v = Some (D.Obj.address_of_field v pos) in
                    let v = D.Obj.field_exn v pos in
                    (* CR mshinwell: Is this the correct [env]? *)
                    find_component ~path:next ~ty ~env
                      ~previous_was_mutable:false
                      ~address_of_v
                      ~what_was_above:Module
                      v
                  end
                end
              | Mty_functor _ -> cannot_handle "Mty_functor"
              | Mty_alias _ -> cannot_handle "Mty_alias"
            end
          | Core_value ->
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
                  find_component ~path:next ~ty:(Cmt_file.Core field_type) ~env
                    ~previous_was_mutable:field_is_mutable
                    ~address_of_v:(Some address_of_field)
                    ~what_was_above:Core_value
                    field
              end
            | _ -> None
            end
          end
        | Project_index { index; next; } ->
          begin match what_was_above with
          | Module ->
            Format.fprintf formatter "@{<error_colour>Error: @}Cannot \
              project by index out of a module\n%!";
            None
          | Core_value ->
            let tuple_like ~element_types ~env =
              let element_types = Array.of_list element_types in
              if index >= D.Obj.size_exn v
                || index >= Array.length element_types
              then
                None
              else
                let address_of_element = D.Obj.address_of_field v index in
                let element = D.Obj.field_exn v index in
                let element_type = element_types.(index) in
                find_component ~path:next ~ty:(Cmt_file.Core element_type) ~env
                  ~previous_was_mutable:false
                  ~address_of_v:(Some address_of_element)
                  ~what_was_above:Core_value
                  element
            in
            begin match oracle_result with
            | Array (element_type, env) ->
              if index >= D.Obj.size_exn v then
                None
              else
                let address_of_element = D.Obj.address_of_field v index in
                let element = D.Obj.field_exn v index in
                find_component ~path:next ~ty:(Cmt_file.Core element_type) ~env
                  ~previous_was_mutable:false
                  ~address_of_v:(Some address_of_element)
                  ~what_was_above:Core_value
                  element
            | Tuple (element_types, env) -> tuple_like ~element_types ~env
            | Ref (element_type, env) ->
              tuple_like ~element_types:[element_type] ~env
            | List (element_type, env) ->
              assert (index >= 0);
              if (not (D.Obj.is_block v))
                || (D.Obj.size_exn v <> 2)
                || (D.Obj.tag_exn v <> 0)
              then
                None
              else if index = 0 then
                let address_of_element = D.Obj.address_of_field v 0 in
                let element = D.Obj.field_exn v 0 in
                find_component ~path:next ~ty:(Cmt_file.Core element_type)
                  ~env ~previous_was_mutable:false
                  ~address_of_v:(Some address_of_element)
                  ~what_was_above:Core_value
                  element
              else
                let address_of_tail = D.Obj.address_of_field v 1 in
                let tail = D.Obj.field_exn v 1 in
                let path = Project_index { index = index - 1; next; } in
                find_component ~path ~ty ~env
                  ~previous_was_mutable:false
                  ~address_of_v:(Some address_of_tail)
                  ~what_was_above:Core_value
                  tail
            | Non_constant_constructor (_, ctor_decls, _, _, env, _) ->
              assert (index >= 0);
              if not (D.Obj.is_block v) then
                None
              else
                let tag = D.Obj.tag_exn v in
                if tag < 0 || tag >= List.length ctor_decls then
                  None
                else
                  let ctor_decl = List.nth ctor_decls tag in
                  let ctor_arg_types =
                    match ctor_decl.cd_args with
                    | Cstr_tuple types -> Array.of_list types
                    | Cstr_record lds ->
                      Array.of_list (
                        List.map (fun (ld : Types.label_declaration) ->
                            ld.ld_type)
                          lds)
                  in
                  if index >= D.Obj.size_exn v
                    || index >= Array.length ctor_arg_types
                  then
                    None
                  else
                    let address_of_element = D.Obj.address_of_field v index in
                    let element = D.Obj.field_exn v index in
                    let element_type = ctor_arg_types.(index) in
                    find_component ~path:next ~ty:(Cmt_file.Core element_type)
                      ~env ~previous_was_mutable:false
                      ~address_of_v:(Some address_of_element)
                      ~what_was_above:Core_value
                      element
            | _ -> None
            end
          end
      in
      find_component ~path ~ty ~previous_was_mutable:false
        ~address_of_v:None ~env ~what_was_above v

  (* CR mshinwell: Remove search path arg *)
  let evaluate t ~path ~lvalue_or_rvalue ~must_be_mutable
        ~cmt_file_search_path:_ ~formatter =
    let (>>=) opt f = match opt with None -> None | Some v -> f v in
    parse ~path
    >>= fun path ->
    if debug then Printf.printf "parsed ok\n%!";
    let name =
      match path with
      | Module { name; _ }
      | Variable { name; _ } -> name
    in
    let what_was_above : what_was_above =
      match path with
      | Module _ -> Module
      | Variable _ -> Core_value
    in
    if debug then Printf.printf "calling Block.get_selected_block\n%!";
    D.Block.get_selected_block ()
    >>= fun block ->
    let rec lookup_symbol block name =
      if debug then Printf.printf "calling lookup_symbol `%s'\n%!" name;
      match D.Block.lookup_symbol block name with
      | Some symbol -> Some symbol
      | None ->
        match D.Block.parent block with
        | None -> None
        | Some block ->
          if debug then Printf.printf "moving to parent block\n%!";
          lookup_symbol block name
    in
    match lookup_symbol block name with
    | None ->
      Format.fprintf formatter "@{<error_colour>Error:@} Cannot find %s \
          @{<variable_name_colour>%s@} \
          in the current block\n%!"
        (match what_was_above with
          | Module -> "module"
          | Core_value -> "variable")
        name;
      None
    | Some symbol ->
      if debug then Printf.printf "calling dwarf_type\n%!";
      D.Symbol.dwarf_type symbol
      >>= fun dwarf_type ->
      let frame = D.Frame.get_selected_frame () in
      let type_and_env =
        Type_helper.type_and_env_from_dwarf_type ~dwarf_type
          ~cmt_cache:t.cmt_cache frame
      in
      let path =
        match path with
        | Module { next; _ }
        | Variable { next; _ } -> next
      in
      if debug then Printf.printf "calling Symbol.value\n%!";
      match D.Symbol.value symbol (Some frame) block with
      | No_value -> None
      | Synthetic_ptr _ -> None  (* See CR above, we should handle this *)
      | Exists_on_target starting_point ->
        evaluate_given_starting_point t ~path
          type_and_env ~lvalue_or_rvalue ~must_be_mutable
          ~formatter ~what_was_above starting_point
end
