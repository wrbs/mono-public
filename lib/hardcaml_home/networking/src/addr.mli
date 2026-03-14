open! Core
open! Hardcaml

module Ip : sig
  type 'a t =
    { ip0 : 'a
    ; ip1 : 'a
    ; ip2 : 'a
    ; ip3 : 'a
    }
  [@@deriving hardcaml, quickcheck]

  module Const : sig
    type nonrec t = Bits.t t [@@deriving string, sexp, compare, equal, quickcheck]
  end
end

module Mac : sig
  type 'a t =
    { mac0 : 'a
    ; mac1 : 'a
    ; mac2 : 'a
    ; mac3 : 'a
    ; mac4 : 'a
    ; mac5 : 'a
    }
  [@@deriving hardcaml, quickcheck]

  module Const : sig
    type nonrec t = Bits.t t [@@deriving string, sexp, compare, equal, quickcheck]
  end
end
