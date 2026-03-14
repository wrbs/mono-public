open! Core
open! Hardcaml

let crc32 = Bits.of_int_trunc ~width:32 0xEDB88320

let update_bit'
  (type c)
  (module C : Comb.S with type t = c)
  bit_in
  ~(polynomial : Bits.t)
  ~crc
  =
  let open C in
  (* Add the incoming bit to the top bit of the current CRC. *)
  let last_bit = lsb crc in
  let add_bit = bit_in ^: last_bit in
  (* Shift in a zero. *)
  srl crc ~by:1
  |> bits_msb
  |> (* Add [add_bit] to every bit where the polynomial is nonzero. *)
  List.map2_exn (Bits.bits_msb polynomial) ~f:(fun polynomial_bit crc_bit ->
    if Bits.to_bool polynomial_bit then add_bit ^: crc_bit else crc_bit)
  |> concat_msb
;;

let update'
  (type c)
  (module C : Comb.S with type t = c)
  bits_in
  ~(polynomial : Bits.t)
  ~crc
  =
  let open C in
  List.fold (bits_lsb bits_in) ~init:crc ~f:(fun crc bit ->
    update_bit' (module C) ~polynomial ~crc bit)
;;

let update = update' (module Signal)
let update_bits = update' (module Bits)

let expected_residual ~polynomial =
  let open Bits in
  let w = width polynomial in
  update_bits (zero w) ~crc:(ones w) ~polynomial
;;

let add_crc32_le packet =
  let open Bits in
  let crc =
    Bits.split_msb packet ~part_width:8
    |> List.fold ~init:(ones 32) ~f:(fun crc bits ->
      update_bits bits ~crc ~polynomial:crc32)
    |> ( ~: )
    |> split_msb ~part_width:8
    |> concat_lsb
  in
  packet @: crc
;;
