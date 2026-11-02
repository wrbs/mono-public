open! Core

type real =
  [ `r1
  | `r2
  | `r3
  | `r4
  | `r5
  | `r6
  | `r7
  | `r8
  | `r9
  | `ra
  | `rb
  | `rc
  | `rd
  | `re
  ]
[@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

type fake =
  [ `r0
  | `rf
  ]
[@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

type t =
  [ real
  | fake
  ]
[@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

val to_int : [< t ] -> int
val of_int : int -> t option
