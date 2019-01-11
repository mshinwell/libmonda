(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2013--2019 Jane Street Group, LLC                        *)
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

module Make (D : Debugger.S) (Cmt_cache : Cmt_cache_intf.S)
      (Type_helper : Type_helper_intf.S
        with module Cmt_cache := Cmt_cache) =
struct
  type t = {
    cmt_cache : Cmt_cache.t;
  }

  let create cmt_cache =
    { cmt_cache;
    }

  let print_given_type_and_env ?variable_name ?always_print formatter
        (type_and_env : (Cmt_file.core_or_module_type * Env.t) option) =
    (* In the cases where the type expression is absent or unhelpful then
       we could print, e.g. " : string" when the value has tag [String_tag].
       However, this might be misleading, in the case where the value is
       of some abstract type (in this example, with a manifest of [string])
       and the user knows the abstract type rather than the manifest.
       What we will do, however, is suppress printing of e.g. " : 'a" since
       it would only seem to serve to clutter.
    *)
    match type_and_env with
    | None -> false
    | Some (Core type_expr, env) ->
      let type_expr = Btype.repr type_expr in
      let print () =
        begin match variable_name with
        | None -> ()
        | Some name ->
          Format.fprintf formatter "@{<variable_name_colour>%s@}" name
        end;
        Format.fprintf formatter " : @{<type_colour>";
        Printtyp.wrap_printing_env ~error:false env (fun () ->
          Printtyp.reset_and_mark_loops type_expr;
          Printtyp.type_expr formatter type_expr);
        Format.fprintf formatter "@}";
        true
      in
      begin match type_expr.desc with
      | Tvar _ | Tunivar _ | Tnil | Tlink _
      | Tsubst _ ->
        begin match always_print with
        | None -> false  (* ignore unhelpful types, as above *)
        | Some () -> print ()
        end
      | Tarrow _ | Ttuple _ | Tconstr _ | Tobject _ | Tfield _ | Tvariant _
      | Tpoly _ | Tpackage _ -> print ()
      end
    | Some (Module modtype, env) ->
      begin match variable_name with
      | None -> ()
      | Some name ->
        Format.fprintf formatter "@{<module_name_colour>%s@}" name
      end;
      Format.fprintf formatter " : @{<type_colour>";
      Printtyp.wrap_printing_env ~error:false env (fun () ->
        Printtyp.modtype formatter modtype);
      Format.fprintf formatter "@}";
      true

  let print ?variable_name t formatter ~dwarf_type =
    let type_and_env =
      match Cmt_cache.find_cached_type t.cmt_cache ~cached_type:dwarf_type with
      | Some type_and_env -> Some type_and_env
      | None ->
        Type_helper.type_and_env_from_dwarf_type ~dwarf_type
          ~cmt_cache:t.cmt_cache
    in
    print_given_type_and_env ?variable_name ~always_print:() formatter
      type_and_env
end
