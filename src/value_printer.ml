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

module List = ListLabels
module Variant_kind = Type_oracle.Variant_kind

let debug = Monda_debug.debug

module Make (D : Debugger.S) = struct
  module Our_type_oracle = Type_oracle.Make (D)

  type t = {
    type_oracle : Our_type_oracle.t;
    cmt_cache : Cmt_cache.t;
  }

  type state = {
    summary : bool;
    depth : int;
    max_depth : int;
    print_sig : bool;
    formatter : Format.formatter;
    max_array_elements_etc_to_print : int;
  }

  let descend state =
    { state with
      depth = state.depth + 1;
      print_sig = false;
    }

  let create ~cmt_cache =
    { type_oracle = Our_type_oracle.create ~cmt_cache;
      cmt_cache;
    }

  let rec value_looks_like_list t value =
    if D.Obj.is_int value && D.Obj.int value = 0 (* nil *) then
      true
    else
      if (not (D.Obj.is_int value))
         && D.Obj.is_block value
         && D.Obj.tag_exn value = 0
         && D.Obj.size_exn value = 2
      then
        value_looks_like_list t (D.Obj.field_exn value 1)
      else
        false

  let print_type_of_value _t ~state ~type_expr_and_env =
    (* In the cases where the type expression is absent or unhelpful then
       we could print, e.g. " : string" when the value has tag [String_tag].
       However, this might be misleading, in the case where the value is
       of some abstract type (in this example, with a manifest of [string])
       and the user knows the abstract type rather than the manifest.
       What we will do, however, is suppress printing of e.g. " : 'a" since
       it would only seem to serve to clutter.
    *)
    let formatter = state.formatter in
    match type_expr_and_env with
    | None -> ()
    | Some (type_expr, _env) ->
      let type_expr = Btype.repr type_expr in
      match type_expr.desc with
      | Tvar _ | Tunivar _ | Tnil | Tlink _
      | Tsubst _ -> ()  (* ignore unhelpful types, as above *)
      | Tarrow _ | Ttuple _ | Tconstr _ | Tobject _ | Tfield _ | Tvariant _
      | Tpoly _ | Tpackage _ ->
        Format.fprintf formatter " : ";
        Printtyp.reset_and_mark_loops type_expr;
        Printtyp.type_expr formatter type_expr

  let rec print_value t ~state ~type_of_ident:type_expr_and_env v : unit =
    let formatter = state.formatter in
    if (state.summary && state.depth > 2)
        || state.depth > state.max_depth then begin
      Format.fprintf formatter ".."
    end else begin
      let formatter = state.formatter in
      match
        Our_type_oracle.find_type_information t.type_oracle ~formatter
          ~type_expr_and_env ~scrutinee:v
      with
      | Obj_unboxed -> print_int t ~state v
      | Obj_unboxed_but_should_be_boxed ->
        (* One common case: a value that is usually boxed but for the moment is
           initialized with [Val_unit].  For example: module fields before
           initializers have been run when using [Closure]. *)
        if D.Obj.int v = 0 then Format.fprintf formatter "()"
        else Format.fprintf formatter "0x%nx" (D.Obj.raw v)
      | Obj_boxed_traversable ->
        if state.summary then Format.fprintf formatter "..."
        else
          let printers =
            Array.init (D.Obj.size_exn v) (fun _ v ->
              print_value t ~state:(descend state) ~type_of_ident:None v)
          in
          generic_printer t ~state ~guess_if_it's_a_list:true ~printers v
      | Obj_boxed_not_traversable ->
        Format.fprintf formatter "<0x%nx, tag %d>" (D.Obj.raw v)
          (D.Obj.tag_exn v)
      | Int -> Format.fprintf formatter "%d" (D.Obj.int v)
      | Char -> print_char t ~state v
      | Abstract path -> Format.fprintf formatter "<%s>" (Path.name path)
      | Array (ty, env) -> print_array t ~state ~ty ~env v
      | List (ty, env) ->
        print_list t ~state ~ty_and_env:(Some (ty, env)) v
      | Ref (ty, env) -> print_ref t ~state ~ty ~env v
      | Tuple (tys, env) -> print_tuple t ~state ~tys ~env v
      | Constant_constructor (name, kind) ->
        print_constant_constructor t ~state ~kind ~name
      | Non_constant_constructor (path, ctor_decls, params,
            instantiated_params, env, kind) ->
        print_non_constant_constructor t ~state ~path ~ctor_decls ~params
            ~instantiated_params ~env ~kind v
      | Record (path, params, args, fields, record_repr, env) ->
        print_record t ~state ~path ~params ~args ~fields
            ~record_repr ~env v
      | Open -> Format.fprintf formatter "<value of open type>"
      | String -> print_string t ~state v
      | Float -> print_float t ~state v
      | Float_array -> print_float_array t ~state v
      | Closure -> print_closure t ~state ~scrutinee:v
      | Lazy -> Format.fprintf formatter "<lazy>"
      | Object -> Format.fprintf formatter "<object>"
      | Abstract_tag -> Format.fprintf formatter "<block with Abstract_tag>"
      | Custom -> print_custom_block t ~state v
    end;
    if state.print_sig then begin
      print_type_of_value t ~state ~type_expr_and_env
    end

  and generic_printer t ~state ?(separator = ",") ?prefix
        ~guess_if_it's_a_list ~(printers : (D.Obj.t -> unit) array)
        value : unit =
    let formatter = state.formatter in
    if guess_if_it's_a_list
        (* If a value, that cannot be identified sensibly from its type,
           looks like a (non-empty) list then we assume it is such.  It seems
           more likely than a set of nested pairs. *)
        && value_looks_like_list t value
    then
      if state.summary then
        Format.fprintf formatter "[...]?"
      else begin
        print_list t ~state ~ty_and_env:None value;
        Format.fprintf formatter "?"
      end
    else begin
      begin match prefix with
      | None -> Format.fprintf formatter "@[<1>[%d: " (D.Obj.tag_exn value)
      (* CR mshinwell: remove dreadful hack *)
      | Some "XXX" -> ()
      | Some p -> Format.fprintf formatter "@[%s " p
      end;
      let original_size = D.Obj.size_exn value in
      let max_size =
        if state.summary then 2 else state.max_array_elements_etc_to_print
      in
      let size, truncated =
        if original_size > max_size then max_size, true
        else original_size, false
      in
      for field = 0 to size - 1 do
        if field > 0 then Format.fprintf formatter "%s@;<1 0>" separator;
        try printers.(field) (D.Obj.field_exn value field)
        with D.Read_error ->
          Format.fprintf formatter "<field %d read failed>" field
      done;
      if truncated then begin
        Format.fprintf formatter "%s <%d elements follow>" separator
            (original_size - max_size)
      end;
      begin match prefix with
      | None -> Format.fprintf formatter "]@]"
      | Some "XXX" -> ()
      | Some _ -> Format.fprintf formatter "@]"
      end
    end

  and print_int _t ~state v =
    let formatter = state.formatter in
    Format.fprintf formatter "%d" (D.Obj.int v)

  and print_char _t ~state v =
    let formatter = state.formatter in
    let value = D.Obj.int v in
    if value >= 0 && value <= 255 then
      Format.fprintf formatter "'%s'" (Char.escaped (Char.chr value))
    else
      Format.fprintf formatter "%nd" (D.Obj.raw v)

  and print_string _t ~state v =
    let formatter = state.formatter in
    let s = D.Obj.string v in
    let max_len = 30 in
    if String.length s > max_len then
      Format.fprintf formatter "%S (* %d chars follow *)"
        (String.sub s 0 max_len) (String.length s - max_len)
    else
      Format.fprintf formatter "%S" (D.Obj.string v)

  and print_tuple t ~state ~tys ~env v =
    let formatter = state.formatter in
    if state.summary && (List.length tys > 2 || state.depth > 0) then
      Format.fprintf formatter "(...)"
    else
      let component_types = Array.of_list tys in
      let size_ok = Array.length component_types = D.Obj.size_exn v in
      let printers =
        Array.map (fun ty v ->
            let type_of_ident = if size_ok then Some (ty, env) else None in
            print_value t ~state:(descend state) ~type_of_ident v)
          component_types
      in
      if List.length tys > 1 then Format.fprintf formatter "(";
      generic_printer t ~state ~printers ~prefix:"XXX"
        ~guess_if_it's_a_list:false v;
      if List.length tys > 1 then Format.fprintf formatter ")"

  and print_array t ~state ~ty ~env v =
    let formatter = state.formatter in
    let size = D.Obj.size_exn v in
    if size = 0 then
      Format.fprintf formatter "@[[| |]@]"
    else if state.summary then
      Format.fprintf formatter "@[[|...|]@]"
    else begin
      Format.fprintf formatter "[| ";
      let printers =
        Array.init size (fun _ v ->
          let type_of_ident = Some (ty, env) in
          print_value t ~state:(descend state) ~type_of_ident v)
      in
      generic_printer t ~state ~separator:";" ~printers ~prefix:"XXX"
        ~guess_if_it's_a_list:false v;
      Format.fprintf formatter " |]"
    end

  and print_list t ~state ~ty_and_env v : unit =
    let formatter = state.formatter in
    let print_element =
      print_value t ~state:(descend state) ~type_of_ident:ty_and_env
    in
    let max_elements =
      if state.summary then 2 else state.max_array_elements_etc_to_print
    in
    let rec aux v ~element_index =
      if D.Obj.is_block v then begin
        if element_index >= max_elements then
          Format.fprintf formatter "@;..."
        else begin
          try
            let elt = D.Obj.field_exn v 0 in
            let next = D.Obj.field_exn v 1 in
            print_element elt;
            if D.Obj.is_block next then Format.fprintf formatter ";@;<1 0>";
            aux next ~element_index:(element_index + 1)
          with D.Read_error ->
            Format.fprintf formatter "<list element read failed>"
        end
      end
    in
    Format.fprintf formatter "@[<hv>[";
    aux v ~element_index:0;
    Format.fprintf formatter "]@]"

  and print_ref t ~state ~ty ~env v =
    let formatter = state.formatter in
    Format.fprintf formatter "ref ";
    print_value t ~state:(descend state) ~type_of_ident:(Some (ty, env))
      (D.Obj.field_exn v 0)

  and print_record t ~state ~path:_ ~params ~args ~fields ~record_repr ~env v =
    let formatter = state.formatter in
    if state.summary then
      Format.fprintf formatter "{...}"
    else if List.length fields <> D.Obj.size_exn v then
      Format.fprintf formatter "{expected %d fields, target has %d}"
        (List.length fields) (D.Obj.size_exn v)
    else begin
      let fields = Array.of_list fields in
      let type_for_field ~index =
        begin match record_repr with
        | Types.Record_float -> Predef.type_float
        | Types.Record_regular ->
          let field_type = fields.(index).Types.ld_type in
          begin try Ctype.apply env params field_type args
          with Ctype.Cannot_apply -> field_type
          end
        | Types.Record_extension
        | Types.Record_inlined _
        | Types.Record_unboxed _ ->
          (* CR mshinwell: fix this *)
          Predef.type_int
        end
      in
      let fields_helpers =
        Array.mapi (fun index ld ->
            let typ = type_for_field ~index in
            let printer v =
              print_value t ~state:(descend state)
                ~type_of_ident:(Some (typ, env)) v
            in
            Ident.name ld.Types.ld_id, printer)
          fields 
      in
      if state.depth = 0 && Array.length fields > 1 then begin
        Format.pp_print_newline formatter ();
        Format.fprintf formatter "@[<v>  "
      end;
      let nb_fields = D.Obj.size_exn v in
      Format.fprintf formatter "@[<v 0>{ ";
      for field_nb = 0 to nb_fields - 1 do
        if field_nb > 0 then Format.fprintf formatter "@   ";
        try
          let v = D.Obj.field_exn v field_nb in
          let (field_name, printer) = fields_helpers.(field_nb) in
          Format.fprintf formatter "@[<2>%s@ =@ " field_name;
          printer v;
          Format.fprintf formatter ";@]"
        with D.Read_error ->
          Format.fprintf formatter "<could not read field %d>" field_nb
      done;
      Format.fprintf formatter "@ }@]";
      if state.depth = 0 && Array.length fields > 1 then begin
        Format.fprintf formatter "@]"
      end
    end

  and print_closure _t ~state ~scrutinee:v =
    let formatter = state.formatter in
    try
      if state.summary then
        Format.fprintf formatter "<fun>"
      else begin
        let partial, pc =
          let arity = D.Obj.int (D.Obj.field_exn v 1) in
          if arity < 2 then
            None, D.Obj.field_as_addr_exn v 0
          else
            let pc = D.Obj.field_as_addr_exn v 2 in
            (* Try to determine if the closure corresponds to a
               partially-applied function. *)
            match D.symbol_at_pc pc with
            | None -> None, pc
            | Some symbol ->
              match Naming_conventions.is_currying_wrapper symbol with
              | None -> None, pc
              | Some (total_num_args, num_args_so_far) ->
                (* Find the original closure in order to determine which
                   function is partially applied.  (See comments about currying
                   functions in cmmgen.ml in the compiler source.) *)
                match begin
                  let v = ref v in
                  for _i = 1 to num_args_so_far do
                    if D.Obj.size_exn !v <> 5 then raise Exit;
                    v := D.Obj.field_exn !v 4;
                    if D.Obj.size_exn !v <> 5 then raise Exit
                  done;
                  !v
                end with
                | exception Exit -> None, pc
                | v ->
                  (* This should be the original function. *)
                  if D.Obj.int (D.Obj.field_exn v 1) <= 1 then
                    None, pc  (* The function should have more than 1 arg. *)
                  else
                    Some (total_num_args, num_args_so_far),
                      D.Obj.field_as_addr_exn v 2
        in
        let partial, partial_args =
          match partial with
          | None -> "", ""
          | Some (total_num_args, args_so_far) ->
            "partial ",
              Printf.sprintf " (got %d of %d args)" args_so_far total_num_args
        in
        match
          D.filename_and_line_number_of_pc pc
            ~use_previous_line_number_if_on_boundary:true
        with
        | None ->
          Format.fprintf formatter "<%sfun> (%a)%s" partial
            D.Target_memory.print_addr pc partial_args
        | Some (filename, Some line) ->
          Format.fprintf formatter "<%sfun> (%s:%d)%s" partial filename line
            partial_args
        | Some (filename, None) ->
          Format.fprintf formatter "<%sfun> (%s, %a)%s" partial filename
            D.Target_memory.print_addr pc partial_args
      end
    with D.Read_error -> Format.fprintf formatter "<closure?>"

  and print_constant_constructor _t ~state ~kind ~name =
    let formatter = state.formatter in
    Format.fprintf formatter "%s%s" (Variant_kind.to_string_prefix kind) name

  and print_non_constant_constructor t ~state ~path:_ ~ctor_decls ~params
        ~instantiated_params ~env ~kind v =
    let formatter = state.formatter in
    let kind = Variant_kind.to_string_prefix kind in
    if debug then begin
      List.iter params ~f:(fun ty ->
        Format.fprintf formatter "param>>";
        Printtyp.reset_and_mark_loops ty;
        Printtyp.type_expr formatter ty;
        Format.fprintf formatter "<<");
      List.iter instantiated_params ~f:(fun ty ->
        Format.fprintf formatter "iparam>>";
        Printtyp.reset_and_mark_loops ty;
        Printtyp.type_expr formatter ty;
        Format.fprintf formatter "<<")
    end;
    let non_constant_ctors, _ =
      List.fold_left ctor_decls
        ~init:([], 0)
        ~f:(fun (non_constant_ctors, next_ctor_number) ctor_decl ->
              let ident = ctor_decl.Types.cd_id in
              match ctor_decl.Types.cd_args with
              | Cstr_tuple [] -> non_constant_ctors, next_ctor_number
              | Cstr_tuple arg_tys ->
                (* CR mshinwell: check [return_type] is just that, and use it.
                   Presumably for GADTs. *)
                (next_ctor_number,
                  (ident, arg_tys))::non_constant_ctors,
                  next_ctor_number + 1
              | Cstr_record label_decls ->
                let arg_tys =
                  List.map label_decls
                    ~f:(fun (label_decl : Types.label_declaration) ->
                      label_decl.ld_type)
                in
                (next_ctor_number,
                  (ident, arg_tys))::non_constant_ctors,
                  next_ctor_number + 1)
    in
    let ctor_info =
      let tag = D.Obj.tag_exn v in
      try Some (List.assoc tag non_constant_ctors) with Not_found -> None
    in
    begin match ctor_info with
    | None ->
      let printers =
        Array.init (D.Obj.size_exn v) (fun _index v ->
          print_value t ~state:(descend state) ~type_of_ident:None v)
      in
      generic_printer t ~state ~printers ~guess_if_it's_a_list:false v
    | Some (cident, args) ->
      if state.summary || List.length args <> D.Obj.size_exn v then begin
        Format.fprintf formatter "%s%s (...)" kind (Ident.name cident)
      end else begin
        let printers =
          let args = Array.of_list args in
          Array.map (fun arg_ty v ->
              if debug then begin
                Format.fprintf formatter "arg>>";
                Printtyp.reset_and_mark_loops arg_ty;
                Printtyp.type_expr formatter arg_ty;
                Format.fprintf formatter "<<"
              end;
              let arg_ty =
                try Ctype.apply env params arg_ty instantiated_params
                with Ctype.Cannot_apply -> arg_ty
              in
              print_value t ~state:(descend state)
                ~type_of_ident:(Some (arg_ty, env)) v)
            args
        in
        let prefix =
          let name = Ident.name cident in
          if List.length args > 1 then Printf.sprintf "%s%s (" kind name
          else name
        in
        generic_printer t ~state ~printers ~prefix
          ~guess_if_it's_a_list:true v;
        if List.length args > 1 then Format.fprintf formatter ")"
      end
    end

  and print_float _t ~state v =
    let formatter = state.formatter in
    try Format.fprintf formatter "%f" (D.Obj.float_field_exn v 0)
    with D.Read_error -> Format.fprintf formatter "<double read failed>"

  and print_float_array _t ~state v =
    let formatter = state.formatter in
    let size = D.Obj.size_exn v in
    if size = 0 then Format.fprintf formatter "@[[| |] (* float array *)@]"
    else if state.summary then Format.fprintf formatter "@[[|...|]@]"
    else begin
      Format.fprintf formatter "@[<1>[| ";
      for i = 0 to size - 1 do
        Format.fprintf formatter "%f" (D.Obj.float_field_exn v i);
        if i < size - 1 then Format.fprintf formatter ";@;<1 0>"
      done;
      Format.fprintf formatter " |] (* float array *)@]"
    end

  and print_custom_block _t ~state v =
    let formatter = state.formatter in
    if D.Obj.size_exn v < 2 then
      Format.fprintf formatter "<malformed custom block>"
    else
      let custom_ops = D.Obj.field_exn v 0 in
      let identifier = D.Obj.c_string_field_exn custom_ops 0 in
      let data_ptr = D.Obj.field_as_addr_exn v 1 in
      match Naming_conventions.examine_custom_block_identifier identifier with
      | Bigarray ->
        Format.fprintf formatter "<Bigarray: data at %a>"
          D.Target_memory.print_addr data_ptr
      | Systhreads_mutex ->
        Format.fprintf formatter "<Mutex.t %a> (* systhreads *)"
          D.Target_memory.print_addr data_ptr
      | Systhreads_condition ->
        Format.fprintf formatter "<Condition.t %a> (* systhreads *)"
          D.Target_memory.print_addr data_ptr
      | Int32 ->
        Format.fprintf formatter "%ld : int32"
          (D.Target_memory.read_int32_exn data_ptr)
      | Int64 ->
        Format.fprintf formatter "%Ld : int64"
          (D.Target_memory.read_int64_exn data_ptr)
      | Channel ->
        (* CR mshinwell: use a better means of retrieving the fd *)
        Format.fprintf formatter "<channel on fd %Ld>"
          (D.Target_memory.read_int64_exn data_ptr)
      | Unknown ->
        Format.fprintf formatter "<custom block '%s' pointing at %a>"
          identifier
          D.Target_memory.print_addr data_ptr

  let print t ~scrutinee ~dwarf_type ~summary ~max_depth
        ~cmt_file_search_path ~formatter =
    if debug then begin
      Format.printf "Value_printer.print %a type %s\n%!"
        D.Obj.print scrutinee dwarf_type
    end;
    let type_of_ident =
      match Cmt_cache.find_cached_type t.cmt_cache ~cached_type:dwarf_type with
      | Some type_expr_and_env -> Some type_expr_and_env
      | None ->
        Type_helper.type_expr_and_env_from_dwarf_type ~dwarf_type
          ~cmt_cache:t.cmt_cache ~cmt_file_search_path
    in
    if debug then Printf.printf "Value_printer.print entry point\n%!";
    Format.fprintf formatter "@[";
    let state = {
      summary;
      depth = 0;
      max_depth;
      print_sig = true;
      formatter;
      (* CR mshinwell: pass this next one as an arg to this function *)
      max_array_elements_etc_to_print = 10;
    }
    in
    print_value t ~state ~type_of_ident scrutinee;
    Format.fprintf formatter "@]";
    Format.pp_print_flush formatter ()
end
