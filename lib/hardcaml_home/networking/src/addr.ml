open! Core
open! Hardcaml

module Ip = struct
  module T = struct
    type 'a t =
      { ip0 : 'a [@bits 8] [@wave_format Unsigned_int]
      ; ip1 : 'a [@bits 8] [@wave_format Unsigned_int]
      ; ip2 : 'a [@bits 8] [@wave_format Unsigned_int]
      ; ip3 : 'a [@bits 8] [@wave_format Unsigned_int]
      }
    [@@deriving hardcaml, quickcheck]
  end

  include T

  module Const = struct
    type t = Bits.t T.t [@@deriving compare, equal]

    let to_string t =
      t
      |> map ~f:(fun b -> Bits.to_unsigned_int b |> Int.to_string)
      |> to_list
      |> String.concat ~sep:"."
    ;;

    let of_string s =
      Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d ->
        let parse n = Bits.of_unsigned_int n ~width:8 in
        { ip0 = parse a; ip1 = parse b; ip2 = parse c; ip3 = parse d })
    ;;

    include functor Sexpable.Of_stringable

    let of_quickcheckable cs = map cs ~f:Bits.of_char
    let to_quickcheckable t = map t ~f:Bits.to_char

    include functor Quickcheckable.Of_quickcheckable (struct
        type t = char T.t [@@deriving quickcheck]
      end)
  end
end

module Mac = struct
  module T = struct
    type 'a t =
      { mac0 : 'a [@bits 8] [@wave_format Hex]
      ; mac1 : 'a [@bits 8] [@wave_format Hex]
      ; mac2 : 'a [@bits 8] [@wave_format Hex]
      ; mac3 : 'a [@bits 8] [@wave_format Hex]
      ; mac4 : 'a [@bits 8] [@wave_format Hex]
      ; mac5 : 'a [@bits 8] [@wave_format Hex]
      }
    [@@deriving hardcaml, quickcheck]
  end

  include T

  module Const = struct
    type t = Bits.t T.t [@@deriving compare, equal]

    let to_string t =
      t
      |> map ~f:(fun b ->
        let n = Bits.to_unsigned_int b in
        Printf.sprintf "%02x" n)
      |> to_list
      |> String.concat ~sep:":"
    ;;

    let of_string s =
      Scanf.sscanf s "%x:%x:%x:%x:%x:%x" (fun a b c d e f ->
        let parse n = Bits.of_unsigned_int n ~width:8 in
        { mac0 = parse a
        ; mac1 = parse b
        ; mac2 = parse c
        ; mac3 = parse d
        ; mac4 = parse e
        ; mac5 = parse f
        })
    ;;

    include functor Sexpable.Of_stringable

    let of_quickcheckable cs = map cs ~f:Bits.of_char
    let to_quickcheckable t = map t ~f:Bits.to_char

    include functor Quickcheckable.Of_quickcheckable (struct
        type t = char T.t [@@deriving quickcheck]
      end)
  end
end
