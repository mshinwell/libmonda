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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make (D : Debugger.S_base) = struct
  include D

  module Value = struct
    type t =
      | Exists_on_target of Obj.t
      | Synthetic_ptr of Synthetic_ptr.t

    let create_exists_on_target obj = Exists_on_target obj
    let create_synthetic_ptr ptr = Synthetic_ptr ptr

    let is_block t =
      match t with
      | Exists_on_target obj -> Obj.is_block obj
      | Synthetic_ptr _ -> true

    let is_int t = not (is_block t)

    let tag_exn t =
      match t with
      | Exists_on_target obj -> Obj.tag_exn obj
      | Synthetic_ptr ptr -> Synthetic_ptr.tag ptr

    let size_exn t =
      match t with
      | Exists_on_target obj -> Obj.size_exn obj
      | Synthetic_ptr ptr -> Synthetic_ptr.size ptr

    let field_exn t index =
      match t with
      | Exists_on_target obj ->
        Some (Exists_on_target (Obj.field_exn obj index))
      | Synthetic_ptr ptr ->
        match Synthetic_ptr.field_exn ptr index with
        | Ok ptr -> Some (Synthetic_ptr ptr)
        | Non_synthetic obj -> Some (Exists_on_target obj)
        | Unavailable -> None

    let field_as_addr_exn t index =
      match t with
      | Exists_on_target obj -> Obj.field_as_addr_exn obj index
      (* CR mshinwell: decide what to do about this *)
      | Synthetic_ptr _ptr -> D.zero_target_addr

    let address_of_field_exn t index =
      match t with
      | Exists_on_target obj -> Some (Obj.address_of_field obj index)
      | Synthetic_ptr _ -> None

    let c_string_field_exn t index =
      match t with
      | Exists_on_target obj -> Obj.c_string_field_exn obj index
      | Synthetic_ptr ptr -> Synthetic_ptr.c_string_field_exn ptr index

    let float_field_exn t index =
      match t with
      | Exists_on_target obj -> Obj.float_field_exn obj index
      | Synthetic_ptr ptr -> Synthetic_ptr.float_field_exn ptr index

    let int t =
      match t with
      | Exists_on_target obj -> Some (Obj.int obj)
      | Synthetic_ptr _ -> None

    let string t =
      match t with
      | Exists_on_target obj -> Obj.string obj
      | Synthetic_ptr ptr -> Synthetic_ptr.string ptr

    let raw t =
      match t with
      | Exists_on_target obj -> Some (Obj.raw obj)
      | Synthetic_ptr _ptr -> None

    let print ppf t =
      match t with
      | Exists_on_target obj -> Obj.print ppf obj
      | Synthetic_ptr ptr -> Synthetic_ptr.print ppf ptr
  end
end
