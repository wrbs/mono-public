open! Core

module T : sig
  type t = private int [@@deriving compare ~localize, equal ~localize]

  val of_int_truncate : int -> t
  val create : int -> int -> int -> t
end = struct
  type t = int [@@deriving compare ~localize, equal ~localize]

  let of_int_truncate n = n land 0xFFFFFF
  let create r g b = ((r land 0xFF) lsl 16) lor ((g land 0xFF) lsl 8) lor (b land 0xFF)
end

include T

let r (t : t) = (t :> int) lsr 16
let g (t : t) = ((t :> int) lsr 8) land 0xFF
let b (t : t) = (t :> int) land 0xFF
let to_string (t : t) = Printf.sprintf "#%06x" (t :> int)
let of_hex s = Scanf.sscanf_opt s "%x" (fun x -> x)

let of_string_opt s =
  let s = String.chop_prefix_if_exists s ~prefix:"#" in
  let%bind.Option r, g, b =
    match String.length s with
    | 3 ->
      let part n =
        let c = String.nget s n in
        String.of_array [| c; c |]
      in
      Some (part 0, part 1, part 2)
    | 6 ->
      let part n = String.sub s ~pos:(2 * n) ~len:2 in
      Some (part 0, part 1, part 2)
    | _ -> None
  in
  let%map.Option r = of_hex r
  and g = of_hex g
  and b = of_hex b in
  create r g b
;;

let of_string s =
  match of_string_opt s with
  | None -> raise_s [%message "RGB.of_string: invalid color code" (s : string)]
  | Some t -> t
;;

include functor Sexpable.Of_stringable [@modality portable]

let caller_identity = Bin_shape.Uuid.of_string "2c32add0-24b5-4fea-b95b-f4995ba9859a"

include functor Binable.Of_stringable_with_uuid [@modality portable]

let create' ~r ~g ~b = create r g b
let parts t = ~r:(r t), ~g:(g t), ~b:(b t)
