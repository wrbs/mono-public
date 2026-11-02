open! Core

module type S = Ints_intf.S

module Make
    (I : sig
       val module_name : string
       val min : int
       val max : int
       val hex_width : int
     end)
    () : S = struct
  type t = int [@@deriving compare, equal]

  let to_int t = t
  let of_int n = Option.some_if (I.min <= n && n <= I.max) n

  let of_int_exn n =
    match of_int n with
    | None -> failwithf "%s.of_int_exn: value out of range %d" I.module_name n ()
    | Some t -> t
  ;;

  let quickcheck_generator = Int.gen_incl I.min I.max
  let to_string t = Int.to_string t
  let of_string s = s |> Int.of_string |> of_int_exn
  let min_int = I.min
  let max_int = I.max

  include functor Sexpable.Of_stringable

  module Hex = struct
    type nonrec t = t [@@deriving compare, equal, quickcheck ~generator]

    let to_string t = Printf.sprintf "%0*x" I.hex_width t
    let of_string s = s |> Int.Hex.of_string |> of_int_exn

    include functor Sexpable.Of_stringable
  end
end

module U4 =
  Make
    (struct
      let module_name = "U4"
      let min = 0x0
      let max = 0xF
      let hex_width = 1
    end)
    ()

module I7 =
  Make
    (struct
      let module_name = "I7"
      let min = -0x40
      let max = 0x3F
      let hex_width = 2
    end)
    ()

module I8 =
  Make
    (struct
      let module_name = "I8"
      let min = -0x80
      let max = 0x7F
      let hex_width = 2
    end)
    ()

module U16 =
  Make
    (struct
      let module_name = "U16"
      let min = 0
      let max = 0xFFFF
      let hex_width = 4
    end)
    ()

module I16 = struct
  include
    Make
      (struct
        let module_name = "I16"
        let min = -0x8000
        let max = 0x7FFF
        let hex_width = 4
      end)
      ()

  let to_u16 t =
    let n = to_int t in
    U16.of_int_exn (if n < 0 then U16.max_int + 1 + n else n)
  ;;

  let of_u16 u16 =
    let n = U16.to_int u16 in
    of_int_exn (if n > max_int then n - (U16.max_int + 1) else n)
  ;;
end
