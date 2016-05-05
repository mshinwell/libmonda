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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Variant_kind = Type_oracle.Variant_kind

let debug = Monda_debug.debug

type t = {
  debugger : (module Debugger.S);
  type_oracle : Type_oracle.t;
}

let create ~debugger ~cmt_cache =
  { debugger;
    type_oracle = Type_oracle.create ~cmt_cache;
  }

let rec value_looks_like_list value =
  if Gdb.Obj.is_int value && Gdb.Obj.int value = 0 (* nil *) then
    true
  else
    if (not (Gdb.Obj.is_int value))
       && Gdb.Obj.is_block value
       && Gdb.Obj.tag value = 0
       && Gdb.Obj.size value = 2
    then
      value_looks_like_list (Gdb.Obj.field value 1)
    else
      false

let print_type_of_print_value ~formatter ~type_expr_and_env =
  (* In the cases where the type expression is absent or unhelpful then
     we could print, e.g. " : string" when the value has tag [String_tag].
     However, this might be misleading, in the case where the value is
     of some abstract type (in this example, with a manifest of [string])
     and the user knows the abstract type rather than the manifest.
     What we will do, however, is suppress printing of e.g. " : 'a" since
     it would only seem to serve to clutter.
  *)
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

let rec print_value t ?(depth=0) ?(print_sig=true) ~summary ~formatter =
      ~type_of_ident:type_expr_and_env v =
  let module D = (val t.debugger : Debugger.S) in
  if (summary && depth > 2) || depth > value_printer_max_depth () then begin
    Format.fprintf formatter ".."
  end else begin
    match
      Type_oracle.find_type_information t.type_oracle ~formatter
          ~type_expr_and_env ~scrutinee:v
    with
    | Obj_unboxed -> print_int ~formatter v
    | Obj_unboxed_but_should_be_boxed ->
      (* One common case: a value that is usually boxed but for the moment is
         initialized with [Val_unit].  For example: module fields before
         initializers have been run when using [Closure]. *)
      if D.Obj.int v = 0 then Format.fprintf formatter "()"
      else Format.fprintf formatter "%Ld" v
    | Obj_boxed_traversable ->
      if summary then Format.fprintf formatter "..."
      else
        let printers =
          Array.init (D.Obj.size v) ~f:(fun _ v ->
            print_value t ~summary ~formatter ~depth:(succ depth) ~print_sig
                ~type_of_ident:None ~summary ~formatter v)
        in
        generic_printer ~printers ~formatter v
    | Obj_boxed_not_traversable ->
      Format.fprintf formatter "<0x%Lx, tag %d>" v (D.Obj.tag v)
    | Char -> print_char ~formatter v
    | Abstract path -> Format.fprintf formatter "<%s>" (Path.name path)
    | Array (ty, env) -> print_array t ~summary ~formatter ~ty ~env v
    | List (ty, env) -> print_list t ~summary ~formatter ~ty ~env v
    | Ref (ty, env) -> print_ref t ~summary ~formatter ~ty ~env v
    | Tuple (tys, env) -> print_tuple t ~summary ~formatter ~tys ~env v
    | Constant_constructor (name, kind) ->
      print_constant_constructor ~formatter ~kind ~name
    | Non_constant_constructor (path, ctor_decls, params,
          instantiated_params, env, kind) ->
      print_non_constant_constructor t ~path ~ctor_decls ~params
          ~instantiated_params ~env ~kind ~formatter v
    | Record (path, params, args, fields, record_repr, env) ->
      print_record t ~summary ~formatter ~path ~params ~args ~fields
          ~record_repr ~env
    | Open -> Format.fprintf formatter "<value of open type>"
    | String -> print_string ~formatter v
    | Float -> print_float ~formatter v
    | Float_array -> print_float_array t ~summary ~formatter v
    | Closure -> print_closure ~summary ~formatter ~scrutinee:v
    | Lazy -> Format.fprintf formatter "<lazy>"
    | Object -> Format.fprintf formatter "<object>"
    | Abstract_tag -> Format.fprintf formatter "<block with Abstract_tag>"
    | Custom -> print_custom_block t ~formatter ~value
  end;
  if print_sig then begin
    print_type_of_print_value ~formatter ~type_expr_and_env
  end

and generic_printer t ?(separator = ",") ?prefix ~guess_if_it's_a_list
      ~printers ~formatter value =
  let module D = (val t.debugger : Debugger.S) in
  if guess_if_it's_a_list
      (* If a value, that cannot be identified sensibly from its type,
         looks like a (non-empty) list then we assume it is such.  It seems
         more likely than a set of nested pairs. *)
      && value_looks_like_list value
  then
    if summary then
      Format.fprintf formatter "[...]?"
    else begin
      let print_element =
        print_print_value ~depth:(succ depth) ~print_sig:false ~formatter
        ~type_of_ident:None ~summary
      in
      list ~print_element ~formatter v;
      Format.fprintf formatter "?"
    end
  else begin
    begin match prefix with
    | None -> Format.fprintf formatter "@[<1>[%d: " (D.Obj.tag value)
    (* CR mshinwell: remove dreadful hack *)
    | Some "XXX" -> ()
    | Some p -> Format.fprintf formatter "@[%s " p
    end;
    let original_size = D.Obj.size value in
    let max_size = D.max_array_etc_elements_to_print () in
    let size, truncated =
      if original_size > max_size then max_size, true
      else original_size, false
    in
    for field = 0 to size - 1 do
      if field > 0 then Format.fprintf formatter "%s@;<1 0>" separator;
      try printers.(field) (D.Obj.field value field)
      with D.Read_error _ ->
        Format.fprintf formatter "<field %d read failed>" field
    done;
    if truncated then begin
      Format.fprintf formatter "%s <%d elements follow>" separator
          (original_size - max_size)
    end;
    begin match prefix with
    | None -> Format.fprintf formatter "]@]"
    | Some "XXX" -> ()
    | Some p -> Format.fprintf formatter "@]"
    end;
    Done
  end

and print_int ~formatter v =
  Format.fprintf formatter "%Ld" v

and print_char ~formatter v =
  let module D = (val t.debugger : Debugger.S) in
  let value = D.Obj.int v in
  if value >= 0 && value <= 255 then
    Format.fprintf formatter "'%s'" (Char.escaped (Char.chr value))
  else
    Format.fprintf formatter "%Ld" v

and print_string ~formatter v =
  let module D = (val t.debugger : Debugger.S) in
  let s = D.Obj.string v in
  let max_len = 30 in
  if String.length s > max_len then
    Format.fprintf formatter "%S (* %d chars follow *)"
      (String.sub s 0 max_len) (String.length s - max_len)
  else
    Format.fprintf formatter "%S" (D.Obj.string v)

and print_tuple t ~summary ~formatter ~tys ~env v =
  let module D = (val t.debugger : Debugger.S) in
  if summary && (List.length tys > 2 || depth > 0) then
    Format.fprintf formatter "(...)"
  else
    let component_types = Array.of_list tys in
    let size_ok = Array.length component_types = D.Obj.size v in
    let printers =
      Array.map component_types ~f:(fun ty v ->
        let type_of_ident = if size_ok then Some (ty, env) else None in
        print_print_value ~depth:(succ depth) ~print_sig:false
          ~type_of_ident ~summary ~formatter v
      )
    in
    if List.length tys > 1 then Format.fprintf formatter "(";
    generic_printer ~printers ~prefix:"XXX" ~force_never_like_list:true
      ~formatter v;
    if List.length tys > 1 then Format.fprintf formatter ")"

and print_array t ~summary ~formatter ~ty ~env v =
  let module D = (val t.debugger : Debugger.S) in
  let size = D.Obj.size v in
  if size = 0 then
    Format.fprintf formatter "@[[| |]@]"
  else if summary then
    Format.fprintf formatter "@[[|...|]@]"
  else begin
    Format.fprintf formatter "[| ";
    let printers =
      Array.init size ~f:(fun _ v ->
        let type_of_ident = Some (ty, env) in
        print_value ~depth:(succ depth) ~print_sig
          ~type_of_ident ~summary ~formatter v
      )
    in
    generic_printer ~separator:";" ~printers ~prefix:"XXX"
      ~force_never_like_list:true ~formatter v;
    Format.fprintf formatter " |]"
  end

and print_list t ~summary ~formatter v =
  let module D = (val t.debugger : Debugger.S) in
  let print_element =
    print_value ~depth:(succ depth) ~print_sig:false ~formatter
      ~type_of_ident:(Some (ty, env)) ~summary
  in
  let rec aux v =
    if D.Obj.is_block v then begin
      try
        let elt = D.Obj.field v 0 in
        let next = D.Obj.field v 1 in
        print_element elt;
        if D.Obj.is_block next then Format.fprintf formatter ";@;<1 0>";
        aux next
      with D.Read_error _ ->
        Format.fprintf formatter "<list element read failed>"
    end
  in
  Format.fprintf formatter "@[<hv>[";
  aux v;
  Format.fprintf formatter "]@]"

and print_ref t ~summary ~formatter ~ty ~env v =
  let module D = (val t.debugger : Debugger.S) in
  Format.fprintf formatter "ref ";
  print_value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
    ~print_sig:false ~summary ~formatter (D.Obj.field v 0)

and print_record t ~summary ~formatter ~path ~params ~args ~fields
      ~record_repr ~env =
  let module D = (val t.debugger : Debugger.S) in
  if summary then
    Format.fprintf formatter "{...}"
  else if List.length fields <> D.Obj.size v then
    Format.fprintf formatter "{expected %d fields, target has %d}"
      (List.length fields) (D.Obj.size v)
  else begin
    let fields = Array.of_list fields in
    let type_for_field ~index =
      begin match record_repr with
      | Types.Record_float -> Predef.type_float
      | Types.Record_regular ->
        let field_type = fields.(index).Types.ld_type in
        try Ctype.apply env params field_type args
        with Ctype.Cannot_apply -> field_type
      end
    in
    let fields_helpers =
      Array.mapi fields ~f:(fun index ld ->
        let typ = type_for_field ~index in
        let printer v =
          print_value ~depth:(succ depth)
            ~type_of_ident:(Some (typ, env))
            ~print_sig:false ~summary ~formatter v
        in
        Ident.name ld.Types.ld_id, printer
      )
    in
    if depth = 0 && Array.length fields > 1 then begin
      Format.pp_print_newline formatter ();
      Format.fprintf formatter "@[<v>  "
    end;
    let nb_fields = D.Obj.size v in
    Format.fprintf formatter "@[<v 0>{ ";
    for field_nb = 0 to nb_fields - 1 do
      if field_nb > 0 then Format.fprintf formatter "@   ";
      try
        let v = D.Obj.field v field_nb in
        let (field_name, printer) = fields_helpers.(field_nb) in
        Format.fprintf formatter "@[<2>%s@ =@ " field_name;
        printer v;
        Format.fprintf formatter ";@]"
      with D.Read_error _ ->
        Format.fprintf formatter "<could not read field %d>" field_nb
    done;
    Format.fprintf formatter "@ }@]";
    if depth = 0 && Array.length fields > 1 then begin
      Format.fprintf formatter "@]"
    end
  end

and print_closure t ~summary ~formatter ~scrutinee:v =
  let module D = (val t.debugger : Debugger.S) in
  try
    if summary then
      Format.fprintf formatter "<fun>"
    else begin
      let pc =
        let name = D.linkage_name_at_pc ~pc:(D.Target.read_field v 0) in
        (* Try to find out the function pointer for the actual function
           in the case that [scrutinee] is actually a currying or
           tuplifying wrapper. *)
        if not (Naming_conventions.is_currying_wrapper name) then
          pc
        else
          (* The "real" function pointer should be the last entry in the
             environment of the closure. *)
          D.Target.read_field v (D.Obj.size v - 1)
      in
      match D.filename_and_line_number_of_pc ~pc with
      | None -> Format.fprintf formatter "<fun> (0x%Lx)" pc
      | Some (filename, Some line) ->
        Format.fprintf formatter "<fun> (%s:%d)" filename line
      | Some (filename, None) ->
        Format.fprintf formatter "<fun> (%s, 0x%Lx)" filename pc
    end
  with D.Read_error _ -> Format.fprintf formatter "<closure?>"

and print_constant_constructor ~formatter ~kind ~name =
  Format.fprintf formatter "%s%s" (Variant_kind.to_string_prefix kind) name

and print_non_constant_constructor t ~path ~ctor_decls ~params
      ~instantiated_params ~env ~kind ~formatter ~value =
  let module D = (val t.debugger : Debugger.S) in
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
            | [] ->
              non_constant_ctors, next_ctor_number
            | _ ->
              (* CR mshinwell: check [return_type] is just that, and use it.
                 Presumably for GADTs. *)
              (next_ctor_number,
                (ident, ctor_decl.Types.cd_args))::non_constant_ctors,
                next_ctor_number + 1)
  in
  let ctor_info =
    let tag = D.Obj.tag v in
    try Some (List.assoc tag non_constant_ctors) with Not_found -> None
  in
  begin match ctor_info with
  | None ->
    generic_printer ~printers:(Lazy.force generic_printers) ~formatter v
  | Some (cident, args) ->
    if summary || List.length args <> D.Obj.size v then begin
      Format.fprintf formatter "%s%s (...)" kind (Ident.name cident)
    end else begin
      let printers =
        let args = Array.of_list args in
        Array.map args ~f:(fun arg_ty v ->
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
          value t ~depth:(succ depth) ~type_of_ident:(Some (arg_ty, env))
            ~print_sig:false ~formatter v
            ~summary
        )
      in
      let prefix =
        let name = Ident.name cident in
        if List.length args > 1 then Printf.sprintf "%s%s (" kind name
        else name
      in
      generic_printer ~printers ~prefix ~formatter v
        ~force_never_like_list:true;
      if List.length args > 1 then Format.fprintf formatter ")"
    end
  end

and print_float t ~formatter ~v =
  let module D = (val t.debugger : Debugger.S) in
  try Format.fprintf formatter "%f" (D.Target.read_double v)
  with D.Read_error _ -> Format.fprintf formatter "<double read failed>"

and print_float_array t ~summary ~formatter v =
  let module D = (val t.debugger : Debugger.S) in
  let size = D.Obj.size v in
  if size = 0 then Format.fprintf formatter "@[[| |] (* float array *)@]"
  else if summary then Format.fprintf formatter "@[[|...|]@]"
  else begin
    Format.fprintf formatter "@[<1>[| ";
    for i = 0 to size - 1 do
      Format.fprintf formatter "%f" (D.Obj.double_field v i);
      if i < size - 1 then Format.fprintf formatter ";@;<1 0>"
    done;
    Format.fprintf formatter " |] (* float array *)@]"
  end

and print_custom_block t ~formatter ~value =
  let module D = (val t.debugger : Debugger.S) in
  if D.Obj.size v < 2 then
    Format.fprintf formatter "<malformed custom block>"
  else
    let custom_ops = D.Obj.field v 0 in
    let identifier = D.Obj.c_string_field custom_ops 0 in
    let data_ptr = D.Obj.field v 1 in
    match Naming_conventions.examine_custom_block_identifier identifier with
    | Bigarray ->
      Format.fprintf formatter "<Bigarray: data at 0x%Lx>" data_ptr
    | Mutex ->
      Format.fprintf formatter "<Mutex.t 0x%Lx> (* systhreads *)" data_ptr
    | Condition ->
      Format.fprintf formatter "<Condition.t 0x%Lx> (* systhreads *)" data_ptr
    | Unknown ->
      Format.fprintf formatter "<custom block '%s' pointing at 0x%Lx>"
        identifier data_ptr

let print t ?depth ?print_sig ~type_of_ident ~summary out v =
  let module D = (val t.debugger : Debugger.S) in
  if debug then Printf.printf "Printer.value entry point\n%!";
  let formatter =
    Format.make_formatter
      (fun str pos len -> D.print out (String.sub str pos len))
      (fun () -> ())
  in
  let print_sig =
    match print_sig with
    | None -> false
    | Some () -> true
  in
  Format.fprintf formatter "@[";
  print_value ?depth ?print_sig ~type_of_ident ~summary ~formatter v;
  Format.fprintf formatter "@]";
  Format.pp_print_flush formatter ()

let print_from_gdb addr stream ~dwarf_type ~call_site ~summary =
  let type_of_ident =
    match Name_laundry.split_base_type_die_name dwarf_type with
    | None -> None
    | Some { output_path; ident_name; } ->
      let output_dir = Filename.dirname output_path in

  in
  let type_of_ident =
    find_type_and_env ~symbol_linkage_name ~cmt_file ~call_site
  in
  print ~depth ~print_sig:true ~type_of_ident ~summary out v

let () = Callback.register "monda_val_print" print_from_gdb
