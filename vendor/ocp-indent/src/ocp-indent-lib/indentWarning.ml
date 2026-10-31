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

let fatal msg = raise (Fatal msg)

let strict = ref false

let emit fmt =
  Format.kasprintf
    (fun msg ->
       if !strict then
         fatal msg
       else
         Format.eprintf "ocp-indent warning: %s\n%!" msg)
    fmt

let set_strict_mode bool = strict := bool
