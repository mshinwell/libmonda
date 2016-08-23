(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2016 Jane Street Group, LLC                              *)
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

(** This module provides functions that navigate around OCaml values. The upper
    levels of such a value may include synthetic parts that are constructed in
    the debugger's address space by virtue of DW_OP_implicit_pointer. Once we
    get into a non-synthetic part, i.e. values that actually exist in the target
    program's memory, we never return to the synthetic world. The value printer
    doesn't need to have any knowledge of which world is being traversed at any
    particular time. *)

module Make (D : Debugger.S) : sig
  type t

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

  (** The address that would be read by [field_exn] for the given field.
      Will return [None] if the supplied [t] references a synthetic
      value in the debugger's address space, rather than on the target. *)
  val address_of_field_exn : t -> int -> target_addr option

  (** Read the NULL-terminated string pointed to by the given field
      from the target's memory. *)
  val c_string_field_exn : t -> int -> string

  (** Read the unboxed float value in the given field from the target's
      memory. *)
  val float_field_exn : t -> int -> float

  (** Assuming that [t] is an integer, return which integer it is.
      Returns [None] if [t] is a pointer to something in the debugger's
      address space. *)
  val int : t -> int option

  (** Assuming that [t] has the layout of a value with tag [String_tag],
      return which string it holds. *)
  val string : t -> string

  (** Return the raw value.  Returns [None] if [t] is a pointer to something
      in the debugger's address space. *)
  val raw : t -> Nativeint.t option

  (** Print the raw value as hex. *)
  val print : Format.formatter -> t -> unit
end
