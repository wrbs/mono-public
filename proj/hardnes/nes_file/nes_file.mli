open! Core

module Mirroring : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp_of]
end

type t =
  { mapper : int
  ; mirroring : Mirroring.t
  ; alternate_nametable : bool
  ; provide_prg_ram : bool
  ; prg_rom : Bigstring.t
  ; chr_rom : Bigstring.t option
  }
[@@deriving sexp_of]

val of_iobuf : (read, Iobuf.seek, Iobuf.global) Iobuf.t -> t Or_error.t
val of_bigstring : Bigstring.t -> t Or_error.t
val of_string : string -> t Or_error.t
