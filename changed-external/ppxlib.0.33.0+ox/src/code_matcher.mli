(** Match source code against generated code *)

open! Import

val match_structure_res :
  pos:Lexing.position ->
  expected:structure ->
  mismatch_handler:(Location.t -> structure -> unit) ->
  structure ->
  (unit, Location.Error.t NonEmptyList.t) result
(** Checks that the given code starts with [expected] followed by [@@@ppxlib.inline.end]
    (or its prefixes; see documentation for [Attribute.declare]).

    Returns an error if there is no [@@@end].

    If some items don't match, it calls [mismatch_handler] with the location of
    the source items and the expected code. *)

val match_structure :
  pos:Lexing.position ->
  expected:structure ->
  mismatch_handler:(Location.t -> structure -> unit) ->
  structure ->
  unit
(** See {!match_structure_res}. Raises a located error in case of error. *)

val match_signature_res :
  pos:Lexing.position ->
  expected:signature_item list ->
  mismatch_handler:(Location.t -> signature_item list -> unit) ->
  signature_item list ->
  (unit, Location.Error.t NonEmptyList.t) result
(** Same for signatures *)

val match_signature :
  pos:Lexing.position ->
  expected:signature_item list ->
  mismatch_handler:(Location.t -> signature_item list -> unit) ->
  signature_item list ->
  unit
(** Same for signatures *)

val allow_deriving_end : bool ref
(** The legacy attribute [@@@deriving.end] is disabled by default, and superseded by
    [@@@ppxlib.inline.end]. It can be enabled by setting [allow_deriving_end := true]. See
    [-allow-deriving-end] in [Driver]. *)
