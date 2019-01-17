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

(** Printer for OCaml values guided by data from the heap and .cmt files. *)

module Make (D : Debugger.S) (Cmt_cache : Cmt_cache_intf.S)
      (Type_helper : Type_helper_intf.S
        with module Cmt_cache := Cmt_cache)
      (Type_printer : Type_printer_intf.S
        with module Cmt_cache := Cmt_cache) :
sig
  type t

  val create
     : Cmt_cache.t
    -> Type_printer.t
    -> t

  val print
     : t
    -> D.Frame.t
    -> scrutinee:D.Value.t
    -> dwarf_type:string
    -> summary:bool
    -> max_depth:int
    -> max_string_length:int
    -> cmt_file_search_path:string list
    -> formatter:Format.formatter
    -> only_print_short_type:bool
    -> only_print_short_value:bool
    -> unit
end
