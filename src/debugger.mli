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

(** Interface between the parts of the library that depend on the
    particular debugger being used and those that do not.

    There is no requirement for an implementation of this library to be
    thread safe.
*)

(** Values that have been read from the program being debugged.
    Analogous to [Obj.t] from the standard library. *)
type obj

(** An address on the target. *)
type target_addr

(** Used for printing on the debugger's terminal. *)
type stream

module type S = sig
  (** Raised when any of the [Obj] or [Target_memory] functions fail to read
      memory. *)
  exception Read_error

  module Obj : sig
    type t = obj

    (** Analogous to [Obj.is_block]---except that [false] is also
        returned if the input is misaligned. *)
    val is_block : t -> bool

    (** Analogous to [Obj.is_int]. *)
    val is_int : t -> bool

    (** Analogous to [Obj.tag].  Reads from the target's memory.
        Returns [Obj.unaligned_tag] if the input is misaligned.
        Returns [Obj.int_tag] if the input satisfies [is_int]. *)
    val tag_exn : t -> int

    (** Analogous to [Obj.size].  Reads from the target's memory. *)
    val size_exn : t -> int

    (** Analogous to [Obj.field].  Reads from the target's memory. *)
    val field_exn : t -> int -> t

    (** Like [field_exn], but for use when the value contains naked
        pointers (e.g. code pointers in closures). *)
    val field_as_addr_exn : t -> int -> target_addr

    (** Read the NULL-terminated string pointed to by the given field
        from the target's memory. *)
    val c_string_field_exn : t -> int -> string

    (** Read the unboxed float value in the given field from the target's
        memory. *)
    val double_field_exn : t -> int -> float

    (** Assuming that [t] is an integer, return which integer it is. *)
    val int : t -> int

    (** Assuming that [t] has the layout of a value with tag [String_tag],
        return which string it holds. *)
    val string : t -> string

    (** Return the raw value. *)
    val raw : t -> Int64.t
  end

  module Target_memory : sig
    type t = target_addr

    (** Read a portion of the target's memory into a buffer. *)
    val read_exn : target_addr -> Bytes.t -> Int64.t -> unit

    (** Read the value in the target's memory at the address given by the
        [target_addr] plus the [offset_in_words]. *)
    val read_value_exn : ?offset_in_words:int -> target_addr -> Obj.t

    (** Like [read_value_exn], but returns a [target_addr]. *)
    val read_addr_exn : ?offset_in_words:int -> target_addr -> target_addr

    (** Read the [Int32.t] in the target's memory at the given address. *)
    val read_int32_exn : target_addr -> Int32.t

    (** Read the [Int64.t] in the target's memory at the given address. *)
    val read_int64_exn : target_addr -> Int64.t

    (** Read the float in the target's memory at the given address. *)
    val read_float_exn : target_addr -> float

    (** Read the unboxed float in the target's memory at the given field
        of the given float array value. *)
    val read_float_field_exn : target_addr -> int -> float

    (** Print an address in hex. *)
    val print_addr : Format.formatter -> t -> unit
  end

  val symbol_at_pc : target_addr -> string option

  (** Given a target address, attempt to determine which filename and line
      number it belongs to. *)
  val filename_and_line_number_of_pc
     : target_addr
    -> use_previous_line_number_if_on_boundary:bool
    -> (string * (int option)) option

  (** The maximum number of elements of a container such as an array to
      print. *)
  val max_array_elements_etc_to_print : unit -> int

  (** The list of compilation directories referenced in the DWARF information
      of the given source file. *)
  val compilation_directories_for_source_file
     : source_filename:string
    -> string list

  (** A formatter that prints to the debugger terminal for user feedback. *)
  val formatter : stream -> Format.formatter
end
