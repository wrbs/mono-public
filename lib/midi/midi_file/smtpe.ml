open! Core

module Fps = struct
  type t =
    | Fps24
    | Fps25
    | Fps29
    | Fps30
  [@@deriving
    sexp_of, compare ~localize, equal ~localize, enumerate, quickcheck ~portable]

  let of_int = function
    | 24 -> Some Fps24
    | 25 -> Some Fps25
    | 29 -> Some Fps29
    | 30 -> Some Fps30
    | _ -> None
  ;;

  let to_int = function
    | Fps24 -> 24
    | Fps25 -> 25
    | Fps29 -> 29
    | Fps30 -> 30
  ;;
end

module Time = struct
  type t =
    { hour : int
    ; minute : int
    ; second : int
    ; frame : int
    ; subframe : int
    ; fps : Fps.t
    }
  [@@deriving
    sexp_of, compare ~localize, equal ~localize, quickcheck ~portable ~observer ~shrinker]

  let create ?(hour = 0) ?(minute = 0) ?(second = 0) ?(frame = 0) ?(subframe = 0) fps =
    let range n v = n >= 0 && n < v in
    if range hour 24
       && range minute 60
       && range second 60
       && range frame (Fps.to_int fps)
       && range subframe 100
    then Some { hour; minute; second; frame; subframe; fps }
    else None
  ;;

  let quickcheck_generator =
    let open Base_quickcheck.Generator.Portable.Let_syntax in
    let%bind fps = Fps.quickcheck_generator in
    let%map hour = Int.gen_incl 0 23
    and minute = Int.gen_incl 0 59
    and second = Int.gen_incl 0 59
    and frame = Int.gen_incl 0 (Fps.to_int fps - 1)
    and subframe = Int.gen_incl 0 99 in
    create fps ~hour ~minute ~second ~frame ~subframe |> Option.value_exn
  ;;

  let of_bytes ~hr ~mn ~se ~fr ~ff =
    let hr = Byte.to_int hr in
    let rr = (hr lsr 5) land 0b11 in
    let hour = hr land 0b11111 in
    let fps : Fps.t =
      match rr with
      | 0b00 -> Fps24
      | 0b01 -> Fps25
      | 0b10 -> Fps29
      | 0b11 -> Fps30
      | _ -> failwith "unreachable by mask"
    in
    create
      fps
      ~hour
      ~minute:(Byte.to_int mn)
      ~second:(Byte.to_int se)
      ~frame:(Byte.to_int fr)
      ~subframe:(Byte.to_int ff)
  ;;
end
