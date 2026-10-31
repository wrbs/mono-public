open Core

type t =
  { signed : bool
  ; i_bits : int
  ; f_bits : int
  }
[@@deriving compare ~localize, equal ~localize, hash]

let width { i_bits; f_bits; signed = _ } = i_bits + f_bits

let to_string ?(space_padding_for_signed = true) { signed; i_bits; f_bits } =
  let u = if signed then if space_padding_for_signed then " " else "" else "u" in
  [%string "%{u}Q%{i_bits#Int}.%{f_bits#Int}"]
;;

let sexp_of_t t = Sexp.Atom (to_string ~space_padding_for_signed:false t)
let precision_re = Re2.create_exn {|^(u?)Q(\d+)\.(\d+)$|}

let t_of_sexp (sexp : Sexp.t) =
  let raise_error () =
    raise_s
      [%message
        "Expected precision sexp to be an atom of the form Q<i>.<f> or uQ<i>.<f>"
          (sexp : Sexp.t)]
  in
  let precision_str =
    match sexp with
    | Atom precision_str -> precision_str
    | _ -> raise_error ()
  in
  match Re2.find_submatches precision_re precision_str with
  | Ok [| _; Some u; Some i; Some f |] ->
    { signed = String.is_empty u; i_bits = Int.of_string i; f_bits = Int.of_string f }
  | _ -> raise_error ()
;;

let bool = { signed = false; i_bits = 1; f_bits = 0 }
let unsigned_int width = { signed = false; i_bits = width; f_bits = 0 }
let signed_int width = { signed = true; i_bits = width; f_bits = 0 }
let is_unsigned_int { signed; f_bits; i_bits = _ } = (not signed) && f_bits = 0
let is_signed_int { signed; f_bits; i_bits = _ } = signed && f_bits = 0
let is_int { signed = _; f_bits; i_bits = _ } = f_bits = 0

let to_signed_exn { signed; i_bits; f_bits } =
  assert (not signed);
  { signed = true; i_bits = i_bits + 1; f_bits }
;;

let min_representable_value_as_float (t : t) =
  if t.signed
  then
    Float.neg
      (Float.of_int (1 lsl (t.i_bits + t.f_bits - 1)) /. Float.of_int (1 lsl t.f_bits))
  else 0.0
;;

let max_representable_value_as_float (t : t) =
  let shift = t.f_bits + if t.signed then t.i_bits - 1 else t.i_bits in
  Float.of_int ((1 lsl shift) - 1) /. Float.of_int (1 lsl t.f_bits)
;;

let gen_random_value_in_precision_as_float ?(must_be_positive = false) (precision : t) =
  let w = width precision in
  if w > 53
  then
    raise_s
      [%message
        "[gen_random_value_in_precision_as_float] supports up to 53 bits of precision. \
         IEEE float64 has 52 mantissa bits. We can only express fixed-point values \
         exactly up to 53 bits (the additional bit come from the hidden bit)"
          (precision : t)];
  let%map.Quickcheck.Generator bits_repr =
    if precision.signed
    then (
      let lower_bound_incl = if must_be_positive then 1 else -(1 lsl (w - 1)) in
      let upper_bound_incl = (1 lsl (w - 1)) - 1 in
      if lower_bound_incl > upper_bound_incl
      then
        raise_s
          [%message
            "Invalid gen_incl range!"
              (must_be_positive : bool)
              (precision : t)
              (lower_bound_incl : int)
              (upper_bound_incl : int)];
      Int.gen_incl lower_bound_incl upper_bound_incl)
    else Int.gen_incl (if must_be_positive then 1 else 0) ((1 lsl w) - 1)
  in
  Float.of_int bits_repr /. Float.of_int (1 lsl precision.f_bits)
;;
