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

module type S = sig
  (** Search order for .cmi files:

      1. The combined "load paths" extracted from all .cmt files loaded thus
         far.  It is generally expected that a particular .cmi will only be
         in one of the directories thus specified; but in any event, order
         is maintained to some extent, according to the code below.
 
      2. Any other source path set in GDB.
 
      Search order for .cmt files:
 
      1. The "prefix name" directory (where the compiler wrote the compilation
         artifacts, possibly set using the "-o" option) as transmitted in
         the DWARF information for the corresponding compilation unit.
 
      2. The search order as for .cmi files, above.
 
      In all cases, for .cmi and .cmt searching and irrespective of where the
      path was obtained, any path used for lookup is subjected to any
      source path substitution rules set in GDB.  Such rules can be used to
      easily describe the relocation of a tree of source files and compiler
      artifacts after the build process.
  *)

  (** Locate the .cmt file for the given compilation unit and load it. *)
  val load_cmt
     : Compilation_unit.t
    -> Cmt_file.t option
end
