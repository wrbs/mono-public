(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(*  ocp-indent is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)


exception Fatal of string

(* Emits a warning with the given formatted message. Prints the warning on
   [stderr] or raise [Fatal msg] in strict mode. *)
val emit : ('a, Format.formatter, unit, unit) format4 -> 'a

(* Set/Unset strict mode. When set to [true] makes all warnings fatal. Strict
   mode is set to [false] by default. *)
val set_strict_mode : bool -> unit
