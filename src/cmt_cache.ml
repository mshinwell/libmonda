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

module Make (LP : Load_path_intf.S) = struct
  type t = {
    cache : Cmt_file.t Compilation_unit.Tbl.t;
    mutable cached_type_counter : int;
    (* CR mshinwell: Use something better than a polymorphic hash table *)
    cached_types : (int, Cmt_file.core_or_module_type * Env.t * Is_parameter.t)
      Hashtbl.t;
  }

  let create () =
    { cache = Compilation_unit.Tbl.create 42;
      cached_type_counter = 0;
      cached_types = Hashtbl.create 42;
    }

  let load t comp_unit =
    match Compilation_unit.Tbl.find t.cache comp_unit with
    | exception Not_found ->
      begin match LP.load_cmt comp_unit with
      | None -> None
      | Some cmt ->
        Compilation_unit.Tbl.add t.cache comp_unit cmt;
        Some cmt
      end
    | cmt -> Some cmt

  let cache_type t (ty : Cmt_file.core_or_module_type) env is_parameter =
    let id = t.cached_type_counter in
    t.cached_type_counter <- t.cached_type_counter + 1;
    assert (not (Hashtbl.mem t.cached_types id));
    let ty : Cmt_file.core_or_module_type =
      match ty with
      | Core type_expr -> Core (Ctype.correct_levels type_expr)
      | Module _ -> ty
    in
    Hashtbl.replace t.cached_types id (ty, env, is_parameter);
    "__ocamlcached " ^ string_of_int id

  let find_cached_type t ~cached_type =
    match String.split_on_char ' ' cached_type with
    | ["__ocamlcached"; id] ->
      begin match int_of_string id with
      | exception _ -> None
      | id ->
        match Hashtbl.find t.cached_types id with
        | exception _ -> None
        | type_expr_and_env -> Some type_expr_and_env
      end
    | _ -> None
end
