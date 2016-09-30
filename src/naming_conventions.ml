(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2015--2016 Jane Street Group, LLC                        *)
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

let is_currying_wrapper name =
  (* CR mshinwell: share names with compilerlibs directly.
     Also add Misc.Stdlib.String.is_prefix *)
  let curry = "caml_curry" in
  let curry_length = min (String.length name) (String.length curry) in
  if String.sub name 0 curry_length <> curry
    || String.length name < curry_length + 3
  then begin
    None
  end else begin
    let remainder =
      String.sub name curry_length (String.length name - curry_length)
    in
    match String.split_on_char '_' remainder with
    | [total_num_args; num_args_so_far] ->
      begin
      try
        let total_num_args = int_of_string total_num_args in
        let num_args_so_far = int_of_string num_args_so_far in
        Some (total_num_args, num_args_so_far)
      with _ -> None
      end
    | _ -> None
  end

type custom_block_identifier =
  | Bigarray
  | Systhreads_mutex
  | Systhreads_condition
  | Int32
  | Int64
  | Nativeint
  | Channel
  | Unknown

let examine_custom_block_identifier = function
  | "_bigarray" -> Bigarray
  | "_mutex" -> Systhreads_mutex
  | "_condition" -> Systhreads_condition
  | "_i" -> Int32
  | "_j" -> Int64
  | "_n" -> Nativeint
  | "_chan" -> Channel
  | _ -> Unknown
