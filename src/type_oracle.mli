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

(** Extraction of type information using data from the heap and .cmt files. *)

(* CR-someday mshinwell: In the future, a more satisfactory approach is
   probably to describe the types of values such that they can be encoded
   in DWARF (which may require some extensions to DWARF).  These types would
   be a halfway house between standard OCaml types and the very limited
   types available by looking at the heap.
   This would require pushing types through the backend along with [Ident.t]
   values, most likely; this could also be used for optimisation. *)

type t

val create
   : cmt_cache:Cmt_cache.t
  -> debugger:(module Debugger.S)
  -> t

module Variant_kind : sig
  type t
  val to_string_prefix : t -> string
end

module Result : sig
  type t = private
    | Obj_boxed_traversable
    | Obj_boxed_not_traversable
    | Obj_unboxed
    | Obj_unboxed_but_should_be_boxed
    | Abstract of Path.t
    | Array of Types.type_expr * Env.t
    | List of Types.type_expr * Env.t
    | Tuple of Types.type_expr list * Env.t
    | Char
    | Int
    | Float
    | Float_array
    | Constant_constructor of string * Variant_kind.t
    | Non_constant_constructor of Path.t * Types.constructor_declaration list
        * Types.type_expr list * Types.type_expr list * Env.t
        * Variant_kind.t
    | Record of Path.t * Types.type_expr list * Types.type_expr list
        * Types.label_declaration list * Types.record_representation
        * Env.t
    | Open
    | Ref of Types.type_expr * Env.t
    | String
    | Closure
    | Lazy
    | Object
    | Abstract_tag
    | Custom

  val to_string : t -> string
end

(** Given a value (either boxed or unboxed) read from the target, known as the
    scrutinee, recover as much type information as possible about the value.
    The starting point is taken to be a pair of a type expression and an
    environment, if available, known to correspond to the value.  (Typically
    the scrutinee would be the value of some named identifier, and the type
    along with its environment would be read from the .cmt file in which that
    identifier was declared.) *)
val find_type_information
   : t
  -> formatter:Format.formatter
  -> type_expr_and_env:(Types.type_expr * Env.t) option
  -> scrutinee:Debugger.obj
  -> Result.t
