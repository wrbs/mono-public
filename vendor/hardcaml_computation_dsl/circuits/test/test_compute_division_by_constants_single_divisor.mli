open Core

val single_divisor_test
  :  dividend_signed:bool
  -> dividend_bits:int
  -> divisor:int
  -> (dividend:int -> unit) Staged.t
