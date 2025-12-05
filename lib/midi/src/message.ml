open! Core

module Kind = struct
  type t =
    | Note_off
    | Note_on
    | Aftertouch
    | Controller
    | Program_change
    | Pressure
    | Pitch_wheel
  [@@deriving
    sexp_of, quickcheck ~portable, compare ~localize, equal ~localize, enumerate]

  module Single = struct
    type t =
      | Program_change
      | Pressure
    [@@deriving
      sexp_of, quickcheck ~portable, compare ~localize, equal ~localize, enumerate]
  end

  module Double = struct
    type t =
      | Note_off
      | Note_on
      | Aftertouch
      | Controller
      | Pitch_wheel
    [@@deriving
      sexp_of, quickcheck ~portable, compare ~localize, equal ~localize, enumerate]
  end

  module Typed = struct
    type _ t =
      | Single : Single.t -> Value.t t
      | Double : Double.t -> (Value.t * Value.t) t

    type packed = T : _ t -> packed
  end

  let to_typed : t -> Typed.packed = function
    | Note_on -> T (Double Note_off)
    | Note_off -> T (Double Note_on)
    | Aftertouch -> T (Double Aftertouch)
    | Controller -> T (Double Controller)
    | Program_change -> T (Single Program_change)
    | Pressure -> T (Single Pressure)
    | Pitch_wheel -> T (Double Pitch_wheel)
  ;;
end

module T = struct
  type t =
    | Note_off of
        { note : Value.t
        ; velocity : Value.t
        }
    | Note_on of
        { note : Value.t
        ; velocity : Value.t
        }
    | Aftertouch of
        { note : Value.t
        ; pressure : Value.t
        }
    | Controller of
        { controller : Value.t
        ; value : Value.t
        }
    | Program_change of Value.t
    | Pressure of Value.t
    | Pitch_wheel of Value.Double.t
  [@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]
end

include T

let payload_length = function
  | Note_on _ -> 2
  | Note_off _ -> 2
  | Aftertouch _ -> 2
  | Controller _ -> 2
  | Program_change _ -> 1
  | Pressure _ -> 1
  | Pitch_wheel _ -> 2
;;

let payload = function
  | Note_on { note; velocity } -> note, Some velocity
  | Note_off { note; velocity } -> note, Some velocity
  | Aftertouch { note; pressure } -> note, Some pressure
  | Controller { controller; value } -> controller, Some value
  | Program_change value -> value, None
  | Pressure value -> value, None
  | Pitch_wheel x ->
    let ~hi, ~lo = Value.Double.to_values x in
    hi, Some lo
;;

let decode_payload (type a) (kind : a Kind.Typed.t) (payload : a) : t =
  match kind, payload with
  | Double Note_on, (note, velocity) -> Note_on { note; velocity }
  | Double Note_off, (note, velocity) -> Note_off { note; velocity }
  | Double Aftertouch, (note, pressure) -> Aftertouch { note; pressure }
  | Double Controller, (controller, value) -> Controller { controller; value }
  | Single Program_change, v -> Program_change v
  | Single Pressure, v -> Pressure v
  | Double Pitch_wheel, (hi, lo) -> Pitch_wheel (Value.Double.of_values ~hi ~lo)
;;

let kind : t -> Kind.t = function
  | Note_off _ -> Note_off
  | Note_on _ -> Note_on
  | Aftertouch _ -> Aftertouch
  | Controller _ -> Controller
  | Program_change _ -> Program_change
  | Pressure _ -> Pressure
  | Pitch_wheel _ -> Pitch_wheel
;;

module Status = struct
  type t = Channel.t * Kind.t
  [@@deriving
    sexp_of, quickcheck ~portable, compare ~localize, equal ~localize, enumerate]

  let to_byte ((channel, kind) : t) =
    let base =
      match kind with
      | Note_off -> '\x80'
      | Note_on -> '\x90'
      | Aftertouch -> '\xA0'
      | Controller -> '\xB0'
      | Program_change -> '\xC0'
      | Pressure -> '\xD0'
      | Pitch_wheel -> '\xE0'
    in
    Byte.(base lor Channel.to_lower_bits channel)
  ;;

  let of_byte b =
    let get (kind : Kind.t) =
      let ch = Channel.of_lower_bits b in
      Some (ch, kind)
    in
    match b with
    | '\x00' .. '\x7F' | '\xF0' .. '\xFF' -> None
    | '\x80' .. '\x8F' -> get Note_on
    | '\x90' .. '\x9F' -> get Note_off
    | '\xA0' .. '\xAF' -> get Aftertouch
    | '\xB0' .. '\xBF' -> get Controller
    | '\xC0' .. '\xCF' -> get Program_change
    | '\xD0' .. '\xDF' -> get Pressure
    | '\xE0' .. '\xEF' -> get Pitch_wheel
  ;;
end

module With_channel = struct
  type nonrec t = Channel.t * T.t
  [@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

  let status (channel, t) = channel, kind t

  let encode ?running_status (channel, msg) ~f =
    let status = status (channel, msg) in
    if [%equal: Status.t option] running_status (Some status)
    then ()
    else f (Status.to_byte status);
    let first, second = payload msg in
    f (Value.to_byte first);
    Option.iter second ~f:(fun x -> f (Value.to_byte x))
  ;;

  let length ?running_status (channel, msg) =
    let status = status (channel, msg) in
    if [%equal: Status.t option] running_status (Some status)
    then payload_length msg
    else 1 + payload_length msg
  ;;

  let to_string ?running_status t =
    let buf = Buffer.create (length ?running_status t) in
    encode ?running_status t ~f:(Buffer.add_char buf);
    Buffer.contents buf
  ;;

  let parse_string ?running_status s =
    let n = String.length s in
    let%bind.Option () = Option.some_if (n >= 1 && n <= 3) () in
    match Value.of_byte (String.nget s 0) with
    | Some v1 ->
      let%bind.Option channel, kind = running_status in
      let (T typed) = Kind.to_typed kind in
      (match typed with
       | Single _ ->
         let%map.Option () = Option.some_if (n = 1) () in
         channel, decode_payload typed v1
       | Double _ ->
         let%bind.Option () = Option.some_if (n = 2) () in
         let%map.Option v2 = Value.of_byte (String.nget s 1) in
         channel, decode_payload typed (v1, v2))
    | None ->
      let%bind.Option channel, kind = Status.of_byte (String.nget s 0) in
      let (T typed) = Kind.to_typed kind in
      (match typed with
       | Single _ ->
         let%bind.Option () = Option.some_if (n = 2) () in
         let%map.Option v1 = Value.of_byte (String.nget s 1) in
         channel, decode_payload typed v1
       | Double _ ->
         let%bind.Option () = Option.some_if (n = 3) () in
         let%bind.Option v1 = Value.of_byte (String.nget s 1) in
         let%map.Option v2 = Value.of_byte (String.nget s 2) in
         channel, decode_payload typed (v1, v2))
  ;;
end
