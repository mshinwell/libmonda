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

type obj = nativeint
type target_addr = nativeint

let zero_target_addr = 0n

external caml_copy_int32 : Obj.t -> Int32.t = "caml_copy_int32"
external caml_copy_int64 : Obj.t -> Int64.t = "caml_copy_int64"
external caml_copy_nativeint : Obj.t -> Nativeint.t = "caml_copy_nativeint"
external caml_copy_double
   : (float [@unboxed])
  -> float
  = "_native_only" "caml_copy_double"
external caml_copy_string : addr:int -> string = "caml_copy_string"

type out_of_heap_buffer = private int

external caml_stat_alloc
   : bytes:(int [@untagged])
  -> out_of_heap_buffer
  = "_native_only" "caml_stat_alloc"

external caml_stat_free
   : out_of_heap_buffer
  -> unit
  = "caml_stat_free" [@@noalloc]

external caml_copy_string_from_out_of_heap_buffer
   : out_of_heap_buffer
  -> string
  = "caml_copy_string"

let dereference_out_of_heap_buffer (buf : out_of_heap_buffer)
      : out_of_heap_buffer =
  Obj.magic (Array.unsafe_get ((Obj.magic buf) : int array) 0)

module Gdb : sig
  (* Bindings directly to gdb. *)

  val target_read_memory
     : addr:target_addr
    -> buf:Bytes.t
    -> size:target_addr
    -> int

  module Ui_file : sig
    type t

    val print_filtered : t -> string -> unit
  end

  (* CR-soon mshinwell: improve this dubious interface *)
  val find_pc_partial_function
     : addr:target_addr
    -> function_name:out_of_heap_buffer
    -> func_start:out_of_heap_buffer
    -> func_end:out_of_heap_buffer
    -> int

  val find_pc_line
     : addr:target_addr
    -> use_previous_line_number_if_on_boundary:int
    -> (string * (int option)) option
end = struct
  external target_read_memory
     : addr:(target_addr [@unboxed])
    -> buf:Bytes.t
    -> size:(target_addr [@unboxed])
    -> (int [@untagged])
    = "_native_only" "target_read_memory" [@@noalloc]

  module Ui_file = struct
    type t = private Int64.t

    external fputs_filtered
       : string
      -> (t [@unboxed])
      -> unit
      = "_native_only" "fputs_filtered" [@@noalloc]

    let print_filtered t text =
      fputs_filtered text t
  end

  external find_pc_partial_function
     : addr:(target_addr [@unboxed])
    -> function_name:out_of_heap_buffer
    -> func_start:out_of_heap_buffer
    -> func_end:out_of_heap_buffer
    -> (int [@untagged])
    = "_native_only" "find_pc_partial_function" [@@noalloc]

  external find_pc_line
     : addr:(target_addr [@unboxed])
    -> use_previous_line_number_if_on_boundary:(int [@untagged])
    -> (string * (int option)) option
    = "_native_only" "monda_find_pc_line"
end

type find_named_value_result =
  | Not_found
  | Found of obj * string

module Gdb_indirect = struct
  (* Bindings to gdb via C stubs in to_gdb.c. *)

  external compilation_directories_for_source_file
     : source_filename:string
    -> string list
    = "monda_compilation_directories_for_source_file"

  external find_named_value
     : name:string
    -> find_named_value_result
    = "monda_find_named_value"

  external find_global_symbol
     : name:string
    -> find_named_value_result
    = "monda_find_global_symbol"
end

let copy_int32 (buf : string) =
  caml_copy_int32 (Obj.field (Obj.repr buf) 0)

let copy_int64 (buf : string) =
  caml_copy_int64 (Obj.field (Obj.repr buf) 0)

let copy_nativeint (buf : string) =
  caml_copy_nativeint (Obj.field (Obj.repr buf) 0)

let copy_nativeint_from_out_of_heap_buffer (buf : out_of_heap_buffer) =
  caml_copy_nativeint (Obj.field (Obj.repr buf) 0)

let copy_float (buf : string) =
  (* CR-soon mshinwell: see if this can be done with float_of_bits etc *)
  caml_copy_double ((Obj.magic buf) : float)

exception Read_error

module Target_memory = struct
  let read_exn addr buf len =
    if String.length buf < len
    then invalid_arg "read_exn: len > String.length buf"
    else begin
      let size = Nativeint.of_int len in
(*
Printf.printf "About to read target memory at 0x%nx (size %nd)\n%!" addr size;
*)
      let result = Gdb.target_read_memory ~addr ~buf ~size in
      if result <> 0 then begin
        raise Read_error
      end
    end

  let priv_buf = Bytes.create 8

  let read_value_exn ?(offset_in_words = 0) addr =
    let addr =
      Nativeint.(add addr (of_int (Arch.size_addr * offset_in_words)))
    in
    read_exn addr priv_buf Arch.size_addr;
    copy_nativeint priv_buf

  let read_addr_exn = read_value_exn

  let read_int32_exn addr =
    read_exn addr priv_buf 4;
    copy_int32 priv_buf

  let read_int64_exn addr =
    read_exn addr priv_buf 8;
    copy_int64 priv_buf

  let read_float_exn addr =
    read_exn addr priv_buf 8;
    copy_float priv_buf

  let read_float_field_exn addr offset =
    read_float_exn Nativeint.(add addr (of_int (8 * offset)))

  let print_addr ppf t =
    Format.fprintf ppf "0x%nx" t
end

let wo_tag w  = Nativeint.(to_int (logand w 0xffn))
let wo_size w = Nativeint.(to_int (shift_right_logical w 10))

module Obj = struct
  let size_addr_minus_one = Nativeint.(of_int (Arch.size_addr - 1))

  type t = obj

  let unit = Nativeint.one

  let is_null x = Nativeint.compare x 0n = 0

  let is_int x = Nativeint.(logand x one = one)
  let is_unaligned x = Nativeint.(logand x size_addr_minus_one <> zero)
  let is_block x = not (is_int x || is_unaligned x)
  
  let field_exn t i =
    assert (i >= (-1));
    Target_memory.read_value_exn t ~offset_in_words:i

  let field_as_addr_exn = field_exn

  let address_of_field t i =
    assert (i >= (-1));
    Nativeint.add t (Nativeint.mul (Nativeint.of_int i)
      (Nativeint.of_int Arch.size_addr))

  let tag_exn x = 
    if is_int x then Obj.int_tag
    else if is_unaligned x then Obj.unaligned_tag
    else wo_tag (field_exn x (-1))

  let size_exn x = wo_size (field_exn x (-1))

  let c_string_field_exn t i =
    let ptr = ref (field_exn t i) in
    let result = ref "" in
    let finished = ref false in
    while not !finished do
      let buf = Bytes.create 1 in
      Target_memory.read_exn !ptr buf 1;
      if String.get buf 0 = '\000' then
        finished := true
      else begin
        result := !result ^ buf;
        ptr := Nativeint.add !ptr (Nativeint.of_int 1)
      end
    done;
    !result

  let float_field_exn t i = Target_memory.read_float_field_exn t i

  let int x = Nativeint.(to_int (shift_right x 1))

  let string x =
    let size = size_exn x * Arch.size_addr in
    let buf = Bytes.create size in
    Target_memory.read_exn x buf size;
    let size = size - 1 - int_of_char buf.[size - 1] in
    String.sub buf 0 size

  let raw t = t

  let print ppf t =
    Format.fprintf ppf "0x%nx" t
end

module Synthetic_ptr = struct
  (* Values of type [t] are pointers to [struct value *] in GDB.
     These values are one word wide and contain a pointer to an OCaml
     value constructed in the debugger's address space. *)

  type t = nativeint

  external value_bits_synthetic_pointer
     : (t [@unboxed])
    -> offset_in_bits:(int [@untagged])
    -> length_in_bits:(int [@untagged])
    -> (int [@untagged])
    = "_native_only" "value_bits_synthetic_pointer" [@@noalloc]

  let is_synthetic t =
    (value_bits_synthetic_pointer t
      ~offset_in_bits:0
      ~length_in_bits:Sys.word_size) <> 0

  external value_as_long
     : (t [@unboxed])
    -> (nativeint [@unboxed])
    = "_native_only" "value_as_long" [@@noalloc]

  (* N.B. [monda_value_struct_elt] does [value_ind] (which can handle implicit
     pointers) before looking up the field name in the resulting struct. *)
  external monda_value_struct_elt
     : (t [@unboxed])
    -> field_name:string
    -> (t [@unboxed])
    = "_native_only" "monda_value_struct_elt" [@@noalloc]

  external value_contents
     : (t [@unboxed])
    -> out_of_heap_buffer
    = "_native_only" "monda_value_contents" [@@noalloc]

  type read_result = Ok of t | Non_synthetic of Obj.t | Unavailable

  let field_exn t index =
    assert (index >= (-1));
    assert (is_synthetic t);
(*Printf.printf ">> field_exn %d on synthetic pointer\n%!" index; *)
    let field_name = string_of_int index in
    let t = monda_value_struct_elt t ~field_name in
    if t = 0n then begin
      Unavailable
    end else begin
(*  Printf.printf "monda_value_struct_elt ok\n%!"; *)
      if is_synthetic t then
        Ok t
      else
        let obj = copy_nativeint_from_out_of_heap_buffer (value_contents t) in
(*  Printf.printf "copy_nativeint gives 0x%nx\n%!" obj;*)
        (* GDB fills unavailable portions of values with zeroes.  We assume that
           an OCaml value will never contain genuine NULL pointers. *)
        if obj = 0n then Unavailable
        else Non_synthetic (obj : Obj.t)
    end

  (* CR mshinwell: improve this; refactor to next level up too *)
  let header t =
    match field_exn t (-1) with
    | Non_synthetic header -> (*Printf.printf "header %nd\n%!" header;*) header
    | Ok _ | Unavailable -> 0n

  let tag t = wo_tag (header t)
  let size t = wo_size (header t)

  (* CR mshinwell: fill these in *)
  let c_string_field_exn _t _index =
    "<unsupported>"

  let float_field_exn _t _index =
    0.0

  let string _t =
    "<unsupported>"

  let print ppf _t =
    Format.fprintf ppf "<synthetic pointer>"
end

let symbol_at_pc pc =
  let function_name = caml_stat_alloc 8 in
  let func_start = caml_stat_alloc 8 in
  let func_end = caml_stat_alloc 8 in
  let result =
    Gdb.find_pc_partial_function ~addr:pc ~function_name
      ~func_start ~func_end
  in
  caml_stat_free func_start;
  caml_stat_free func_end;
  let result =
    if result = 0 then None
    else
      Some (caml_copy_string_from_out_of_heap_buffer
        (dereference_out_of_heap_buffer function_name))
  in
  caml_stat_free function_name;
  result

let filename_and_line_number_of_pc addr
      ~use_previous_line_number_if_on_boundary =
  Gdb.find_pc_line ~addr
    ~use_previous_line_number_if_on_boundary:
      (if use_previous_line_number_if_on_boundary then 1 else 0)

let compilation_directories_for_source_file =
  Gdb_indirect.compilation_directories_for_source_file

let find_named_value =
  Gdb_indirect.find_named_value

let find_global_symbol =
  Gdb_indirect.find_global_symbol

type stream = Gdb.Ui_file.t

let formatter (stream : stream) =
  Format.make_formatter (fun str pos len ->
      Gdb.Ui_file.print_filtered stream (String.sub str pos len))
    (fun () -> ())

(* CR-someday mshinwell: Things to add:
  - GC-safe watchpoints
  - resurrect old code for compile+run
*)
