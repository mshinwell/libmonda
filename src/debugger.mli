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

(** Interface between the parts of the library that depend on the
    particular debugger being used and those that do not. *)

type obj

module type S = sig

  (* Values that have been read from the program being debugged. *)
  module Obj : sig
    type t = obj

    exception Read_error

    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
    val is_block : t -> bool
  end

  (* Access to the memory of the program being debugged. *)
  module Target : sig
    val read_field : Obj.t -> int -> Obj.t
  end

(*
  (* Display of text to the user of the debugger. *)
  val print : string -> unit
  module Stream : sig
    type t
    val to_formatter : t -> Format.formatter
  end

  (* The linkage name of the function containing the program counter
     value [pc]. *)
  val linkage_name_at_pc : pc:Obj.t -> string

  (* As much information as known about the filename and line number
     of the source text that was compiled to program counter [pc]. *)
  val filename_and_line_number_of_pc
     : pc:Obj.t
    -> (string * (int option)) option

  (* User preferences set in the debugger. *)
  val max_array_etc_elements : unit -> int
  val max_depth : unit -> int
  val cmt_search_path : unit -> string list
*)
end
