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

type t = {
  mutable primary_search_path : string list;
  mutable secondary_search_path : string list;
  cache : (string, Cmt_file.t) Hashtbl.t;
}

let create () =
  { primary_search_path = [];
    secondary_search_path = [];
    cache = Hashtbl.create 42;
  }

(* CR mshinwell: check digests of .cmt files *)

let clear_search_paths t =
  t.primary_search_path <- [];
  t.secondary_search_path <- []

let set_primary_search_path t search_path =
  t.primary_search_path <- search_path

let set_secondary_search_path t search_path =
  t.secondary_search_path <- search_path

let get_primary_search_path t = t.primary_search_path

let search_path t = t.primary_search_path @ t.secondary_search_path

let load ?expected_in_directory t ~leafname =
  try Some (Hashtbl.find t.cache leafname)
  with Not_found ->
    let search_path =
      match expected_in_directory with
      | None -> search_path t
      | Some expected_in_directory ->
        expected_in_directory :: (search_path t)
    in
    match Misc.find_in_path_uncap search_path leafname with
    | exception Not_found -> None
    | pathname ->
      match
        Cmt_file.load ~pathname
          ~primary_search_path_for_dependencies:t.primary_search_path
      with
      | None -> None
      | Some cmt ->
        Hashtbl.add t.cache leafname cmt;
        Some cmt
