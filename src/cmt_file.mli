(**************************************************************************)
(*                                                                        *)
(*                Make OCaml native debugging awesome!                    *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(* Copyright (c) 2013--2018 Jane Street Group, LLC                        *)
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

(** Representation of a single .cmt (typed tree) file. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Load a .cmt file from the given input channel.  The [filename] is only
    used for error reporting. *)
val load_from_channel_then_close
   : filename:string
  -> in_channel
  -> add_to_load_path:(string list -> unit)
  -> t option

val add_information_from_cmi_file : t -> unit_name:string -> t

type core_or_module_type =
  | Core of Types.type_expr
  | Module of Types.module_type

(** Find the type and environment of definition of the given identifier by
    looking through a .cmt file.  The [name] and [stamp] will typically be
    obtained from a DWARF type using [Type_helper]. *)
val type_of_ident
   : t
  -> name:string
  -> stamp:int
  -> (core_or_module_type * Env.t) option

val type_of_call_site_argument
   : t
  -> line:int
  -> column:int
  -> index:int
  -> (core_or_module_type * Env.t) option

(* CR mshinwell: replace this crap with a variant *)
val distinguished_var_name : string

val cmt_infos : t -> Cmt_format.cmt_infos
