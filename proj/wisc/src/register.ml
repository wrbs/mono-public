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

let to_int : [< t ] -> int = function
  | `r0 -> 0x0
  | `r1 -> 0x1
  | `r2 -> 0x2
  | `r3 -> 0x3
  | `r4 -> 0x4
  | `r5 -> 0x5
  | `r6 -> 0x6
  | `r7 -> 0x7
  | `r8 -> 0x8
  | `r9 -> 0x9
  | `ra -> 0xa
  | `rb -> 0xb
  | `rc -> 0xc
  | `rd -> 0xd
  | `re -> 0xe
  | `rf -> 0xf
;;

let of_int = function
  | 0x0 -> Some `r0
  | 0x1 -> Some `r1
  | 0x2 -> Some `r2
  | 0x3 -> Some `r3
  | 0x4 -> Some `r4
  | 0x5 -> Some `r5
  | 0x6 -> Some `r6
  | 0x7 -> Some `r7
  | 0x8 -> Some `r8
  | 0x9 -> Some `r9
  | 0xa -> Some `ra
  | 0xb -> Some `rb
  | 0xc -> Some `rc
  | 0xd -> Some `rd
  | 0xe -> Some `re
  | 0xf -> Some `rf
  | _ -> None
;;
