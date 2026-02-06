open! Core

type 'a t =
  { data : 'a [@bits -1]
  ; valid : 'a
  ; error : 'a
  }
[@@deriving hardcaml ~pre]

include functor Dynamic_interface.Of_deriving_hardcaml_pre
