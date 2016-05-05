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
type target_addr

module type S = sig

  (* Values that have been read from the program being debugged. *)
  module Obj : sig
    type t = obj

    (** Analogous to [Obj.is_block]. *)
    val is_block : t -> bool

    (** Analogous to [Obj.is_int]. *)
    val is_int : t -> bool

    (** Analogous to [Obj.tag]. *)
    val tag : t -> int

    (** Analogous to [Obj.size]. *)
    val size : t -> int

    (** Analogous to [Obj.field]. *)
    val field : t -> int -> t

    (** Read the NULL-terminated string pointed to by the given field. *)
    val c_string_field : t -> int -> string

    (** Read the unboxed float value in the given field. *)
    val double_field : t -> int -> double

    (** Assuming that [t] is an integer, return which integer it is. *)
    val int : t -> int

    (** Assuming that [t] has the layout of a value with tag [String_tag],
        return which string it holds. *)
    val string : t -> string
  end

  module Target_memory : sig
    exception Read_error

    (** Read a portion of the target's memory into a buffer. *)
    val read_exn : target_addr -> Bytes.t -> Int64.t -> unit

    (** Read the value in the target's memory at the address given by the
        [target_addr] plus the [offset_in_words]. *)
    val read_value_exn : ?offset_in_words:int -> target_addr -> Obj.t

    (** Read the [Int32.t] in the target's memory at the given address. *)
    val read_int32_exn : target_addr -> Int32.t

    (** Read the [Int64.t] in the target's memory at the given address. *)
    val read_int64_exn : target_addr -> Int64.t

    (** Read the float in the target's memory at the given address. *)
    val read_float_exn : target_addr -> float

    (** Read the unboxed float in the target's memory at the given field
        of the given float array value. *)
    val read_float_field_exn : target_addr -> int -> float
  end

  (** Given a target address, attempt to determine which filename and line
      number it belongs to. *)
  val filename_and_line_number_of_pc
     : target_addr
    -> use_previous_line_number_if_on_boundary:bool
    -> (string * int) option

  (** The maximum number of elements of a container such as an array to
      print. *)
  val max_array_elements_etc_to_print : unit -> int

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
