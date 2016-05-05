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

module M = Main.Make (struct
  (* Basic GDB types *)
  type addr = int64
  type gdb_stream

  module Priv = struct
    (* GDB primitive functions *)
    external find_in_path : string -> string option = "ml_gdb_find_in_path"

    external target_read_memory 
      : addr -> string -> int -> int 
      = "ml_gdb_target_read_memory"
    
    external copy_double
      : string -> float
      = "ml_gdb_copy_double"
    
    external copy_int64
      : string -> int64
      = "ml_gdb_copy_int64"
    
    external copy_int32
      : string -> int32
      = "ml_gdb_copy_int32"

    external gdb_print_filtered 
      : gdb_stream -> string -> unit 
      = "ml_gdb_print_filtered"

    type symtab
    type symtab_and_line = {
      symtab : symtab;
      line : int option;
      addr_pc : addr;
      addr_end : addr;
    }

    external gdb_function_linkage_name_at_pc
      : addr -> string option
      = "ml_gdb_function_linkage_name_at_pc"

    external gdb_find_pc_line
      : addr -> not_current:bool -> symtab_and_line option
      = "ml_gdb_find_pc_line"

    external gdb_symtab_filename
      : symtab -> string
      = "ml_gdb_symtab_filename"

    (* Expected type of callbacks *)
    type callback_print_val
      = addr -> gdb_stream -> unit
    type callback_demangle
      = string -> int -> string
  end

  let print = Priv.gdb_print_filtered
  let print_endline out = print out "\n"
  let printf stream = Printf.ksprintf (print stream)

  exception Read_error of int

  module Target = struct
    module Value = Int64
    let arch_wo_size = 8
    let wo_tag w  = Value.(to_int (logand w 255L))
    let wo_size w = Value.(to_int (shift_right_logical w 10))

    let read_memory_exn addr buf len =
      if String.length buf < len
      then invalid_arg "read_memory: len > String.length buf"
      else 
        let errcode = Priv.target_read_memory addr buf len in
        if errcode <> 0 then raise (Read_error errcode)

    let priv_buf = Bytes.create 8
    
    let read_memory addr priv_buf len =
      try read_memory_exn addr priv_buf len;
          `Ok
      with Read_error n -> `Errcode n
    
    let read_int32 addr =
      read_memory_exn addr priv_buf 4;
      Priv.copy_int32 priv_buf

    let read_int64 addr =
      read_memory_exn addr priv_buf 8;
      Priv.copy_int64 priv_buf

    let read_value = read_int64

    let read_double addr =
      read_memory_exn addr priv_buf 8;
      Priv.copy_double priv_buf

    let read_field addr offset = 
      read_value Value.(add addr (of_int (arch_wo_size * offset)))

    let read_double_field addr offset =
      read_double Value.(add addr (of_int (8 * offset)))
  end

  module Target_obj = struct
    open Target
    let arch_wo_size_minus_one = Value.(of_int (Target.arch_wo_size - 1))

    type t = addr

    let lazy_tag         = Obj.lazy_tag        
    let closure_tag      = Obj.closure_tag     
    let object_tag       = Obj.object_tag      
    let infix_tag        = Obj.infix_tag       
    let forward_tag      = Obj.forward_tag     
    let no_scan_tag      = Obj.no_scan_tag     
    let abstract_tag     = Obj.abstract_tag    
    let string_tag       = Obj.string_tag      
    let double_tag       = Obj.double_tag      
    let double_array_tag = Obj.double_array_tag
    let custom_tag       = Obj.custom_tag      
    let int_tag          = Obj.int_tag         
    let unaligned_tag    = Obj.unaligned_tag   

    (* Not correctly handled *)
    let out_of_heap_tag  = Obj.out_of_heap_tag         

    let is_int       x = Value.(logand x one = one)
    let is_unaligned x = Value.(logand x arch_wo_size_minus_one <> zero)

    let tag x = 
      if is_int x then int_tag
      else if is_unaligned x then unaligned_tag
      else try Target.wo_tag (read_field x (-1) )
           with Read_error n -> out_of_heap_tag

    let is_block x = not (is_int x || is_unaligned x)

    let size x = Target.wo_size (read_field x (-1))
    
    let field t i = Target.read_field t i
    let double_field t i = Target.read_double_field t i

    let add_offset t i = Value.(add t (of_int32 i))

    let int x = Value.(to_int (shift_right x 1))
    let string x = (* CR mshinwell: need more descriptive names *)
      let size = size x * Target.arch_wo_size in
      let buf = Bytes.create size in
      Target.read_memory_exn x buf size;
      let size = size - 1 - int_of_char buf.[size - 1] in
      String.sub buf 0 size

    (* Read a NULL-terminated string that is pointed to by field [i] of the value
       (or structure with equivalent layout) at address [t]. *)
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

  module Obj = Target_obj


              (D.Priv.gdb_symtab_filename symtab) line
          | Some {D.Priv. symtab} ->
            Format.fprintf formatter "<fun> (%s, 0x%Lx)"
              (D.Priv.gdb_symtab_filename symtab)
              pc



  let formatter ~stream =
    Format.make_formatter
      (fun str pos len -> Gdb.print stream (String.sub str pos len))
      (fun () -> ())

end)

let () =
  Callback.register "monda_val_print" M.print_value
