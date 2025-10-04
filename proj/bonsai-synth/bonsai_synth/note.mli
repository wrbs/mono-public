open! Core

module Letter : sig
  type t =
    | C
    | CS
    | D
    | DS
    | E
    | F
    | FS
    | G
    | GS
    | A
    | AS
    | B
  [@@deriving sexp, string, compare, hash, enumerate]

  val is_sharp : t -> bool
end

type t = Letter.t * int [@@deriving sexp, string]

include Comparable.S with type t := t
include Hashable.S with type t := t

val letter : t -> Letter.t
val octave : t -> int
val frequency : ?a:float -> t -> float

(** Creators *)

val middle_c : t
val midi_bottom : t
val midi_top : t
val of_int : int -> t
val to_int : t -> int
val to_midi_value_exn : t -> Midi_value.t
val of_midi_value : Midi_value.t -> t

(** Functions *)

(** Renders as a 3-length string.

    Raises if octave out of range [0, 9] *)
val to_aligned_string_exn : t -> string

val transpose : ?semis:int -> ?octaves:int -> t -> t
