(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2013--2018 Jane Street Group, LLC                        *)
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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type obj = nativeint
type target_addr = nativeint

let zero_target_addr = 0n

(* Note: GDB functions may not be called directly from this file; they need
   to go through a wrapper (in to_gdb.c) that catches any GDB exceptions
   that may be thrown.  (Also, GDB now uses C++ so it is unlikely that any
   functions can be easily called directly, due to name mangling.) *)

external caml_copy_int32 : Obj.t -> Int32.t = "caml_copy_int32"
external caml_copy_int64 : Obj.t -> Int64.t = "caml_copy_int64"
external caml_copy_nativeint : Obj.t -> Nativeint.t = "caml_copy_nativeint"
external caml_copy_double
   : (float [@unboxed])
  -> float
  = "_native_only" "caml_copy_double"
(*external caml_copy_string : addr:int -> bytes = "caml_copy_string"*)

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
  -> bytes
  = "caml_copy_string"

let dereference_out_of_heap_buffer (buf : out_of_heap_buffer)
      : out_of_heap_buffer =
  Obj.magic (Array.unsafe_get ((Obj.magic buf) : int array) 0)

module Gdb : sig
  (* Bindings directly to gdb. *)
  (* CR mshinwell: These are not direct bindings any more *)

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
    = "_native_only" "monda_target_read_memory" [@@noalloc]

  module Ui_file = struct
    type t = private Int64.t

    external fputs_filtered
       : string
      -> (t [@unboxed])
      -> unit
      = "_native_only" "monda_fputs_filtered" [@@noalloc]

    let print_filtered t text =
      fputs_filtered text t
  end

  external find_pc_partial_function
     : addr:(target_addr [@unboxed])
    -> function_name:out_of_heap_buffer
    -> func_start:out_of_heap_buffer
    -> func_end:out_of_heap_buffer
    -> (int [@untagged])
    = "_native_only" "monda_find_pc_partial_function" [@@noalloc]

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

  type raw_ocaml_specific_compilation_unit_info = {
    compiler_version : string option;
    unit_name : string option;
    config_digest : string option;
    prefix_name : string option;
    linker_dirs : string option;
  }

  external raw_ocaml_specific_compilation_unit_info
     : unit_name:string
    -> raw_ocaml_specific_compilation_unit_info option
    = "monda_ocaml_specific_compilation_unit_info"

  external add_search_path
     : dirname:string
    -> unit
    = "monda_add_search_path"

  external find_and_open
     : filename:string
    -> dirname:string option
    -> (string * Unix.file_descr) option
    = "monda_find_and_open"

  external find_named_value
     : name:string
    -> find_named_value_result
    = "monda_find_named_value"

  external find_global_symbol
     : name:string
    -> find_named_value_result
    = "monda_find_global_symbol"
end

let copy_int32 (buf : Bytes.t) =
  caml_copy_int32 (Obj.field (Obj.repr buf) 0)

let copy_int64 (buf : Bytes.t) =
  caml_copy_int64 (Obj.field (Obj.repr buf) 0)

let copy_nativeint (buf : Bytes.t) =
  caml_copy_nativeint (Obj.field (Obj.repr buf) 0)

let copy_nativeint_from_out_of_heap_buffer (buf : out_of_heap_buffer) =
  caml_copy_nativeint (Obj.field (Obj.repr buf) 0)

let copy_float (buf : Bytes.t) =
  (* CR-soon mshinwell: see if this can be done with float_of_bits etc *)
  caml_copy_double ((Obj.magic buf) : float)

exception Read_error

module Target_memory = struct
  let read_exn addr buf len =
    if Bytes.length buf < len
    then invalid_arg "read_exn: len > String.length buf"
    else begin
      let size = Nativeint.of_int len in
(*
Printf.printf "About to read target memory at 0x%nx (size %nd)\n%!" addr size;
*)
      let result = Gdb.target_read_memory ~addr ~buf ~size in
      if result <> 0 then begin
        Printf.eprintf "Target memory read at 0x%nx (size %nd) failed\n%!"
          addr size;
        for i = 0 to Arch.size_addr do
          Bytes.set buf i '\000'
        done
(*
        raise Read_error
*)
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
      if Bytes.get buf 0 = '\000' then
        finished := true
      else begin
        result := !result ^ (Bytes.to_string buf);
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
    let size = size - 1 - int_of_char (Bytes.get buf (size - 1)) in
    String.sub (Bytes.to_string buf) 0 size

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
    = "_native_only" "monda_value_bits_synthetic_pointer" [@@noalloc]

  let is_synthetic t =
    (value_bits_synthetic_pointer t
      ~offset_in_bits:0
      ~length_in_bits:Sys.word_size) <> 0

(*
  external value_as_long
     : (t [@unboxed])
    -> (nativeint [@unboxed])
    = "_native_only" "monda_value_as_long" [@@noalloc]
*)

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
  let function_name = caml_stat_alloc ~bytes:8 in
  let func_start = caml_stat_alloc ~bytes:8 in
  let func_end = caml_stat_alloc ~bytes:8 in
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
  match result with
  | None -> None
  | Some result -> Some (Bytes.to_string result)

let add_search_path = Gdb_indirect.add_search_path

type ocaml_specific_compilation_unit_info = {
  compiler_version : string;
  unit_name : Ident.t;
  config_digest : Digest.t;
  prefix_name : string;
  linker_dirs : string list;
}

let ocaml_specific_compilation_unit_info ~unit_name
      : ocaml_specific_compilation_unit_info option =
  let unit_name = Ident.name unit_name in
  let unit_info =
    Gdb_indirect.raw_ocaml_specific_compilation_unit_info ~unit_name
  in
  match unit_info with
  | None -> None
  | Some unit_info ->
    let config_digest =
      match unit_info.config_digest with
      | None -> None
      | Some digest ->
        match Digest.from_hex digest with
        | exception (Invalid_argument _) -> None
        | digest -> Some digest
    in
    match
      unit_info.compiler_version,
      unit_info.unit_name,
      config_digest,
      unit_info.prefix_name
    with
    | Some compiler_version, Some unit_name, Some config_digest,
        Some prefix_name ->
      let linker_dirs =
        match unit_info.linker_dirs with
        | None -> []
        | Some linker_dirs ->
          Dwarf_name_laundry.demangle_linker_dirs linker_dirs
      in
      Some {
        compiler_version;
        unit_name = Ident.create_persistent unit_name;
        config_digest;
        prefix_name;
        linker_dirs;
      }
    | _, _, _, _ -> None

let find_and_open ~filename ~dirname =
  match Gdb_indirect.find_and_open ~filename ~dirname with
  | None -> None
  | Some (filename, fd) -> Some (filename, Unix.in_channel_of_descr fd)

let filename_and_line_number_of_pc addr
      ~use_previous_line_number_if_on_boundary =
  Gdb.find_pc_line ~addr
    ~use_previous_line_number_if_on_boundary:
      (if use_previous_line_number_if_on_boundary then 1 else 0)

let find_named_value =
  Gdb_indirect.find_named_value

let find_global_symbol =
  Gdb_indirect.find_global_symbol

type stream = Gdb.Ui_file.t

(*
exception Debugger_formatter_stopped
*)

module Gdb_style = struct
  type t =
    | Normal
    | File_name
    | Function_name
    | Variable_name
    | Address
    | Type
    | Error

  external to_ansi_escape : t -> string
    = "_native_only" "monda_gdb_style_to_ansi_escape"
end

let intercept_style_tags_on_formatter ppf =
  let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
  let mark_open_stag stag =
    let gdb_style : Gdb_style.t option =
      match stag with
      | Format.String_tag "file_name_colour" -> Some File_name
      | Format.String_tag "function_name_colour" -> Some Function_name
      | Format.String_tag "variable_name_colour" -> Some Variable_name
      | Format.String_tag "address_colour" -> Some Address
      | Format.String_tag "type_colour" -> Some Type
      | Format.String_tag "error_colour" -> Some Error
      | _ -> None
    in
    match gdb_style with
    | None -> stag_functions.mark_open_stag stag
    | Some gdb_style -> Gdb_style.to_ansi_escape gdb_style
  in
  let mark_close_stag stag =
    let was_gdb_style =
      match stag with
      | Format.String_tag "file_name_colour"
      | Format.String_tag "function_name_colour"
      | Format.String_tag "variable_name_colour"
      | Format.String_tag "address_colour"
      | Format.String_tag "type_colour"
      | Format.String_tag "error_colour" -> true
      | _ -> false
    in
    if not was_gdb_style then stag_functions.mark_close_stag stag
    else Gdb_style.to_ansi_escape Normal
  in
  let stag_functions =
    { stag_functions with
      mark_open_stag;
      mark_close_stag;
    }
  in
  Format.pp_set_tags ppf true;
  Format.pp_set_mark_tags ppf true;
  Format.pp_set_formatter_stag_functions ppf stag_functions

let formatter (stream : stream) =
  let ppf =
    Format.make_formatter (fun str pos len ->
        let str = String.sub str pos len in
        Gdb.Ui_file.print_filtered stream str)
      (fun () -> ())
  in
  intercept_style_tags_on_formatter ppf;
  ppf

external monda_get_chars_printed_on_current_line : unit -> int
  = "_native_only" "monda_get_chars_printed_on_current_line"

external monda_wrap_column : unit -> int
  = "_native_only" "monda_wrap_column"

(* CR mshinwell: This is only intended to be temporary to work around
   gdb master's awful formatting. *)
external monda_set_wrap_column : int -> unit
  = "_native_only" "monda_set_wrap_column"

let with_formatter_margins ppf ~summary f =
  if not summary then begin
    let wrap_column = monda_wrap_column () in
    if wrap_column > 0 then begin
      Format.pp_set_margin ppf wrap_column;
    end;
    let indent = monda_get_chars_printed_on_current_line () in
    Format.pp_open_vbox ppf (indent + 2);
    f ppf;
    Format.pp_close_box ppf ()
  end else begin
    monda_set_wrap_column 0;
    Format.pp_open_hbox ppf ();
    Format.pp_set_margin ppf max_int;
    f ppf;
    Format.pp_close_box ppf ()
  end

(* CR-someday mshinwell: Things to add:
  - GC-safe watchpoints
  - resurrect old code for compile+run
*)
