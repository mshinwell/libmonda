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

type cmt = Cmi_format.cmi_infos option * Cmt_format.cmt_infos option

type t = {
  search_path : (unit -> string list);
  mutable most_recent_search_path : string list option;
  cache : (string, cmt) Hashtbl.t;
}

let create ~search_path =
  { search_path;
    most_recent_search_path = None;
    cache = Hashtbl.create 42;
  }

(* Since we don't yet check digests of .cmt files, we flush the cache
   whenever the search path is changed. *)
let check_for_search_path_updates t =
  let current_search_path = (t.search_path) () in
  let should_flush =
    match t.most_recent_search_path with
    | None -> true
    | Some most_recent_search_path ->
      current_search_path <> most_recent_search_path
  in
  if should_flush then begin
    Hashtbl.clear t.cache;
    t.most_recent_search_path <- Some current_search_path
  end

let read t ~unit_name =
  check_for_search_path_updates t;
  try Some (Hashtbl.find t.cache unit_name)
  with Not_found ->
    match t.most_recent_search_path with
    | None -> assert false
    | Some search_path ->
      match Misc.find_in_path_uncap search_path (unit_name ^ ".cmt") with
      | exception Not_found -> None
      | pathname ->
        match Cmt_format.read pathname with
        | exception _exn -> None  (* CR mshinwell: improve error reporting *)
        | cmt ->
          Hashtbl.add t.cache unit_name cmt;
          Some cmt
