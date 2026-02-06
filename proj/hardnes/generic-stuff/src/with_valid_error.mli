open! Core

type bits_param = int

type 'a t =
  { data : 'a
  ; valid : 'a
  ; error : 'a
  }

include Dynamic_interface.S with type 'a t := 'a t and type bits_param := bits_param
