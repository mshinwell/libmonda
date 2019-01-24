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

(** Interface between the parts of the library that depend on the
    particular debugger being used and those that do not.

    There is no requirement for an implementation of this library to be
    thread safe.
*)

module type S_base = sig
  (** Raised when any of the [Obj] or [Target_memory] functions fail to read
      memory. *)
  exception Read_error

  (** An address on the target. *)
  type target_addr

  val zero_target_addr : target_addr

  module Obj : sig
    (** OCaml values that have been read from the program being debugged.
        Analogous to [Obj] in the standard library. *)

    type t

    (** The unit value. *)
    val unit : t

    (** Analogous to [Obj.is_block]---except that [false] is also
        returned if the input is misaligned. *)
    val is_block : t -> bool

    (** Analogous to [Obj.is_int]. *)
    val is_int : t -> bool

    val is_null : t -> bool

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

    (** The address that would be read by [field_exn] for the given field. *)
    val address_of_field : t -> int -> target_addr

    (** Read the NULL-terminated string pointed to by the given field
        from the target's memory. *)
    val c_string_field_exn : t -> int -> string

    (** Read the unboxed float value in the given field from the target's
        memory. *)
    val float_field_exn : t -> int -> float

    (** Assuming that [t] is an integer, return which integer it is. *)
    val int : t -> int

    (** Assuming that [t] has the layout of a value with tag [String_tag],
        return which string it holds. *)
    val string : t -> string

    (** Return the raw value. *)
    val raw : t -> Nativeint.t

    include Identifiable.S with type t := t
  end

  module Synthetic_ptr : sig
    (** Pointers to OCaml "synthetic values" that have been constructed in the
        debugger's address space from DWARF information. *)

    type t

    val tag : t -> int
    val size : t -> int

    (** Read a field of a synthetic value.  This may yield another synthetic
        pointer; a normal (Obj.t) pointer that actually exists on the target;
        or that the relevant field is unavailable. *)
    type read_result = Ok of t | Non_synthetic of Obj.t | Unavailable
    val field_exn : t -> int -> read_result

    val c_string_field_exn : t -> int -> string
    val float_field_exn : t -> int -> float
    val string : t -> string

    include Identifiable.S with type t := t
  end

  module Target_memory : sig
    (** Read a portion of the target's memory into a buffer. *)
    val read_exn : target_addr -> Bytes.t -> int -> unit

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
    val print_addr : Format.formatter -> target_addr -> unit
  end

  val symbol_at_pc : target_addr -> string option

  (** Given a target address, attempt to determine which filename and line
      number it belongs to. *)
  val filename_and_line_number_of_pc
     : target_addr
    -> use_previous_line_number_if_on_boundary:bool
    -> (string * (int option)) option

  type ocaml_specific_compilation_unit_info = private {
    compiler_version : string;
    unit_name : Ident.t;
    config_digest : Digest.t;
    prefix_name : string;
    linker_dirs : string list;
  }

  module Call_site : sig
    type t

    (** An address within the call instruction. *)
    val pc : t -> target_addr

    val line_number : t -> int option

    val column_number : t -> int option

    (** The OCaml-specific unit info at the call site. *)
    val ocaml_specific_compilation_unit_info
       : t
      -> ocaml_specific_compilation_unit_info option

    (** Return the OCaml-formatted DWARF type of the given function argument
        (numbered left to right starting from zero, following [Is_parameter] in
        the compiler) at the given [call_site] in the context of the given
        [frame].

        The [call_site] address is the value of the program counter as seen
        having unwound to the [frame] from the callee, which may be just after
        the call. (As returned by e.g. [Frame.caller], above.) *)

    val dwarf_type_of_argument
       : t
      -> index:int
      -> string option
  end

  module Frame : sig
    type t

    val none : t

    val get_selected_frame : unit -> t

    type caller_result = private
      | No_caller
      | Caller of t * Call_site.t

    (** Return the call site in the frame F that called the selected frame (not
        the return address of the current frame), together with F. This PC value
        may not always be known, for example in some tail call scenarios; in
        these cases, [None] is returned. *)
    val caller : t -> caller_result
  end

  (** Extract values for the given compilation unit that are transmitted via
      OCaml-specific DWARF attributes. *)
  val ocaml_specific_compilation_unit_info
     : unit_name:Ident.t
    -> ocaml_specific_compilation_unit_info option

  (** Add the given directory to the front of the debugger's search path
      (to be used for [find_and_open], below). *)
  val add_search_path : dirname:string -> unit

  (** Find and open the file by the name of [filename] expected to be in
      directory [dirname]. A channel and the resolved filename is returned.

      This function applies the various search path rules, potentially including
      substitution rules, of the debugger. The file in question might be a
      source file or compilation artifact. The user is responsible for closing
      the channel.
  *)
  val find_and_open
     : filename:string
    -> dirname:string option
    -> (string * in_channel) option

  type find_named_value_result =
    | Not_found
    | Found of Obj.t * string

  (** Attempt to resolve a name (e.g. the name of a parameter or
      let-bound variable) to a value and its DWARF type. *)
  (* CR mshinwell: This doesn't seem useful *)
  val find_named_value : name:string -> find_named_value_result

  (** Attempt to resolve a name to a global symbol (e.g. one corresponding
      to an Flambda-lifted allocated constant). *)
  val find_global_symbol : name:string -> find_named_value_result

  type stream

  (** A formatter that prints to the debugger terminal for user feedback. *)
  val formatter
     : stream
    -> Format.formatter

  (** Calls the given function having set up the given [formatter] to
      correspond to the current margin settings in the debugger. *)
  val with_formatter_margins
     : Format.formatter
    -> summary:bool
    -> (Format.formatter -> 'a)
    -> 'a
end

module type S = sig
  include S_base

  (** This module provides functions that navigate around OCaml values. The
      upper levels of such a value may include synthetic parts that are
      constructed in the debugger's address space by virtue of
      DW_OP_implicit_pointer. Once we get into a non-synthetic part, i.e. values
      that actually exist in the target program's memory, we never return to the
      synthetic world. The value printer doesn't need to have any knowledge of
      which world is being traversed at any particular time. *)
  module Value : sig
    type t

    (** Create a value given an address in the target program's memory. *)
    val create_exists_on_target : Obj.t -> t

    (** Create a value given a GDB [struct value*] representing a synthetic
        pointer.  (No checks are made on the value.) *)
    val create_synthetic_ptr : Synthetic_ptr.t -> t

    val is_null : t -> bool

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

    (** Analogous to [Obj.field].  Reads from the target's memory.
        Returns [None] if the value is unavailable. *)
    val field_exn : t -> int -> t option

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
    (* CR mshinwell: This should return the integer in the second case, no? *)
    val int : t -> int option

    (** Assuming that [t] has the layout of a value with tag [String_tag],
        return which string it holds. *)
    val string : t -> string

    (** Return the raw value.  Returns [None] if [t] is a pointer to something
        in the debugger's address space. *)
    val raw : t -> Nativeint.t option

    (** Print the raw value as hex. *)
    val print : Format.formatter -> t -> unit

    include Identifiable.S with type t := t
  end
end
