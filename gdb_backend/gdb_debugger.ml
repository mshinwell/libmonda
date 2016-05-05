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

type obj = int64



type addr = int64
type gdb_stream

module To_gdb = struct
  external find_in_path : string -> string option
    = "monda_gdb_find_in_path"

  external target_read_memory : addr -> string -> int -> int 
    = "monda_gdb_target_read_memory" "noalloc"

  external gdb_print_filtered : gdb_stream -> string -> unit 
    = "monda_gdb_print_filtered" "noalloc"

  type symtab

  type symtab_and_line = {
    symtab : symtab;
    line : int option;
    addr_pc : addr;
    addr_end : addr;
  }

  external gdb_function_linkage_name_at_pc
    : addr -> string option
    = "monda_gdb_function_linkage_name_at_pc"

  external gdb_find_pc_line
    : addr -> not_current:bool -> symtab_and_line option
    = "monda_gdb_find_pc_line"

  external gdb_symtab_filename
    : symtab -> string
    = "monda_gdb_symtab_filename"
end

external caml_copy_int32 : Obj.t -> Int32.t = "caml_copy_int32"
external caml_copy_int64 : Obj.t -> Int64.t = "caml_copy_int64"
external caml_copy_double : Obj.t -> float = "caml_copy_double"

let copy_int32 (buf : string) =
  caml_copy_int32 (Obj.field (Obj.repr buf) 0)

let copy_int64 (buf : string) =
  caml_copy_int64 (Obj.field (Obj.repr buf) 0)

let copy_double (buf : string) =
  caml_copy_double (Obj.field (Obj.repr buf) 0)

let print = Priv.gdb_print_filtered
let print_endline out = print out "\n"
let printf stream = Printf.ksprintf (print stream)

exception Read_error

module Target = struct
  module Value = Int64

  let wo_tag w  = Value.(to_int (logand w 255L))
  let wo_size w = Value.(to_int (shift_right_logical w 10))

  let read_memory_exn addr buf len =
    if String.length buf < len
    then invalid_arg "read_memory_exn: len > String.length buf"
    else To_gdb.target_read_memory_exn addr buf len

  let priv_buf = Bytes.create 8
  
  let read_int32 addr =
    read_memory_exn addr priv_buf 4;
    Priv.copy_int32 priv_buf

  let read_int64 addr =
    read_memory_exn addr priv_buf 8;
    Priv.copy_int64 priv_buf

  let read_value ?(offset_in_words = 0) addr =
    read_int64 Value.(add addr (of_int (Arch.size_addr * offset_in_words)))

  let read_double addr =
    read_memory_exn addr priv_buf 8;
    Priv.copy_double priv_buf

  let read_double_field addr offset =
    read_double Value.(add addr (of_int (8 * offset)))
end

module Obj = struct
  let Arch.size_addr_minus_one = Value.(of_int (Target.Arch.size_addr - 1))

  type t = addr

  let is_int       x = Value.(logand x one = one)
  let is_unaligned x = Value.(logand x Arch.size_addr_minus_one <> zero)

  let tag x = 
    if is_int x then int_tag
    else if is_unaligned x then unaligned_tag
    else try Target.wo_tag (read_field x (-1))
         with Read_error n -> out_of_heap_tag

  let is_block x = not (is_int x || is_unaligned x)

  let size x = Target.wo_size (read_field x (-1))
  
  let field t i = Target.read_field t i
  let double_field t i = Target.read_double_field t i

  let add_offset t i = Value.(add t (of_int32 i))

  let int x = Value.(to_int (shift_right x 1))

  let string x =
    let size = size x * Target.Arch.size_addr in
    let buf = Bytes.create size in
    Target.read_memory_exn x buf size;
    let size = size - 1 - int_of_char buf.[size - 1] in
    String.sub buf 0 size

  let c_string_field t i =
    let ptr = ref (field t i) in
    let result = ref "" in
    let finished = ref false in
    while not !finished do
      let buf = Bytes.create 1 in
      read_memory_exn !ptr buf 1;
      if String.get buf 0 = '\000' then
        finished := true
      else begin
        result := !result ^ buf;
        ptr := Int64.add !ptr (Int64.of_int 1)
      end
    done;
    !result
end


let formatter ~stream =
  Format.make_formatter
    (fun str pos len -> Gdb.print stream (String.sub str pos len))
    (fun () -> ())

let () =
  Callback.register "monda_val_print" M.print_value
