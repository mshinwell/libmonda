(***************************************************************************)
(*                                                                         *)
(*                 Make OCaml native debugging awesome!                    *)
(*                                                                         *)
(*                   Mark Shinwell, Jane Street Europe                     *)
(*                                                                         *)
(*  Copyright (c) 2018--2019 Jane Street Group, LLC                        *)
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

let debug = Monda_debug.debug

module Make (D : Debugger.S) = struct
  (* CR mshinwell: Extend to handle cyclic values. *)
  (* CR mshinwell: Provide proper return values (e.g. "too large"). *)

  module V = D.Value

  let rec copy0 (v : V.t) : 'a =
    if debug then begin
      Format.printf "Value_copier.copy0 %a\n%!" V.print v
    end;
    if V.is_int v then begin
      if debug then begin
        Format.printf "Value is an immediate.\n%!"
      end;
      match V.int v with
      | None ->
        (* See CR in debugger.mli *)
        failwith "Synthetic integer value: not yet implemented"
      | Some i -> Obj.repr i
    end else begin
      if debug then begin
        Format.printf "Value is a pointer.\n%!"
      end;
      assert (V.is_block v);
      let tag = V.tag_exn v in
      let size = V.size_exn v in
      if debug then begin
        Format.printf "Tag %d size %d.\n%!" tag size
      end;
      if size < 0 || size > 1024 * 1024 then begin
        failwith "Outsize value"
      end;
      let block = Obj.new_block tag size in
      if tag < Obj.no_scan_tag then begin
        for field = 0 to size - 1 do
          match V.field_exn v field with
          | None -> failwith "Value only partially available"
          | Some field_contents ->
            Obj.set_field block field (copy0 field_contents)
        done
      end else begin
        if tag = Obj.double_tag || tag = Obj.double_array_tag
          || tag = Obj.string_tag
        then begin
          for field = 0 to size - 1 do
            match V.field_exn v field with
            | None -> failwith "Value only partially available"
            | Some field_contents ->
              match V.raw field_contents with
              | None -> failwith "Cannot get raw value of field"
              | Some field_contents ->
                D.write_nativeint_into_field field_contents block ~field
          done
        end else begin
          (* CR mshinwell: This should just return "can't copy" rather than
             bombing out *)
          failwith "Cannot copy value"
        end
      end;
      block
    end

  let copy v = Obj.magic (copy0 v)
end
