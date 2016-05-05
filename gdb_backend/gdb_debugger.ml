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

type obj = nativeint
type target_addr = nativeint
type stream = private int  (* only ever constructed by gdb itself *)

external caml_copy_int32 : Obj.t -> Int32.t = "caml_copy_int32"
external caml_copy_int64 : Obj.t -> Int64.t = "caml_copy_int64"
external caml_copy_double : Obj.t -> float = "caml_copy_double"
external caml_copy_string : addr:int -> string = "caml_copy_string"

module Gdb : sig
  (* Bindings directly to gdb. *)

  val target_read_memory
     : addr:target_addr
    -> buf:Bytes.t
    -> size:target_addr
    -> int

  module Ui_file : sig
    type t = private int

    val print_filtered : t -> string -> unit
  end

  module Symtab : sig
    type t = private int

    val filename : t -> string
  end

  module Symtab_and_line : sig
    type t = private int

    val symtab : t -> Symtab.t option
    val line : t -> int option
  end

  val find_pc_line
     : addr:target_addr
    -> use_previous_line_number_if_on_boundary:bool
    -> Symtab_and_line.t
end = struct
  (* Type "int" is used to conceal naked pointers from the GC. *)

  external int_as_pointer : int -> 'a = "%int_as_pointer"

  let null : int = int_as_pointer 0

  external target_read_memory
     : addr:(target_addr [@unboxed])
    -> buf:Bytes.t
    -> size:(target_addr [@unboxed])
    -> (int [@untagged])
    = "target_read_memory" "noalloc"

  module Ui_file = struct
    type t = int

    external print_filtered : t -> format_str:string -> string -> unit
      = "fprintf_filtered" "noalloc"

    let print_filtered t text =
      print_filtered t "%s" text
  end

  module Symtab = struct
    type t = int

    let filename t : string =
      caml_copy_string ~addr:(((Obj.magic t) : int array).(3))
  end

  module Symtab_and_line = struct
    type t = int

    let symtab t : Symtab.t option =
      let symtab = ((Obj.magic t) : int array).(1) in
      if symtab == null then None
      else Some symtab

    let line t : int option =
      let line = ((Obj.magic t) : int array).(3) * 2 in
      if line < 1 then None else Some line
  end

  external find_pc_line
     : addr:(target_addr [@unboxed])
    -> use_previous_line_number_if_on_boundary:(bool [@unboxed])
    -> Symtab_and_line.t
    = "find_pc_line" "noalloc"
end

module Gdb_indirect = struct
  (* Bindings to gdb via C stubs in to_gdb.c. *)

  external compilation_directories_for_source_file
     : source_filename:string
    -> string list
    = "monda_compilation_directories_for_source_file"
end

let copy_int32 (buf : string) =
  caml_copy_int32 (Obj.field (Obj.repr buf) 0)

let copy_int64 (buf : string) =
  caml_copy_int64 (Obj.field (Obj.repr buf) 0)

let copy_double (buf : string) =
  caml_copy_double (Obj.field (Obj.repr buf) 0)

exception Read_error

module Target = struct
  module Value = Int64

  let wo_tag w  = Value.(to_int (logand w 0xffn))
  let wo_size w = Value.(to_int (shift_right_logical w 10))

  let read_memory_exn addr buf len =
    if String.length buf < len
    then invalid_arg "read_memory_exn: len > String.length buf"
    else begin
      let result = Gdb.target_read_memory ~addr ~buf ~size in
      if result <> 0 then begin
        raise Read_error
      end
    end

  let priv_buf = Bytes.create 8
 
  let read_int32_exn addr =
    read_memory_exn addr priv_buf 4;
    copy_int32 priv_buf

  let read_int64_exn addr =
    read_memory_exn addr priv_buf 8;
    copy_int64 priv_buf

  let read_value_exn ?(offset_in_words = 0) addr =
    read_int64 Value.(add addr (of_int (Arch.size_addr * offset_in_words)))

  let read_double_exn addr =
    read_memory_exn addr priv_buf 8;
    copy_double priv_buf

  let read_double_field_exn addr offset =
    read_double Value.(add addr (of_int (8 * offset)))
end

module Obj = struct
  let size_addr_minus_one = Value.(of_int (Arch.size_addr - 1))

  type t = addr

  let is_int x = Value.(logand x one = one)
  let is_unaligned x = Value.(logand x Arch.size_addr_minus_one <> zero)
  let is_block x = not (is_int x || is_unaligned x)
  
  let field t i =
    Target.read_value t ~offset_in_words:i

  let tag_exn x = 
    if is_int x then Obj.int_tag
    else if is_unaligned x then Obj.unaligned_tag
    else Target.wo_tag (read_field x (-1))

  let size_exn x = Target.wo_size (read_field x (-1))

  let c_string_field_exn t i =
    let ptr = ref (field t i) in
    let result = ref "" in
    let finished = ref false in
    while not !finished do
      let buf = Bytes.create 1 in
      Target.read_memory_exn !ptr buf 1;
      if String.get buf 0 = '\000' then
        finished := true
      else begin
        result := !result ^ buf;
        ptr := Int64.add !ptr (Int64.of_int 1)
      end
    done;
    !result

  let double_field_exn t i = Target.read_double_field_exn t i

  let int x = Value.(to_int (shift_right x 1))

  let string x =
    let size = size x * Arch.size_addr in
    let buf = Bytes.create size in
    Target.read_memory_exn x buf size;
    let size = size - 1 - int_of_char buf.[size - 1] in
    String.sub buf 0 size
end

let compilation_directories_for_source_file =
  Gdb_indirect.compilation_directories_for_source_file

let formatter stream =
  Format.make_formatter (fun str pos len ->
      Gdb.Ui_file.print_filtered stream (String.sub str pos len))
    (fun () -> ())

let () =
  Callback.register "monda_val_print" M.print_value
