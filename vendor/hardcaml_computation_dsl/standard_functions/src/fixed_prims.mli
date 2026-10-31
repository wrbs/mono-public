open Hardcaml
module Fixed_signed : module type of Hardcaml_fixed_point.Signed (Signal)

(** Provides various constants and simulation helper functions for converting Fixed_signed
    into bits and floats *)

module Make (Config : sig
    val i_bits : int
    val f_bits : int
  end) : sig
  module With_overflow : sig
    type t =
      { value : Fixed_signed.t
      ; overflow : Signal.t
      }
  end

  val of_float : float -> Fixed_signed.t

  (** Convert a Signal.t which has an integer and fraction part into a Fixed_signed.t *)
  val fp : Signal.t -> Fixed_signed.t

  (** Maximum and minimum values representable. *)
  val max_value : Fixed_signed.t

  val min_value : Fixed_signed.t

  (** Constant 0 *)
  val fp_zero : Fixed_signed.t

  (** Constant 1 *)
  val fp_one : Fixed_signed.t

  (** Constant -1 *)
  val fp_neg_one : Fixed_signed.t

  (** Constant 0.5 *)
  val fp_half : Fixed_signed.t

  (** Constant -0.5 *)
  val fp_neg_half : Fixed_signed.t

  (** Divide a Fixed_signed.t by 2 *)
  val fp_div_by_2 : Fixed_signed.t -> Fixed_signed.t

  (** Resize a Fixed_signed.t into this width, but without checking for overflow. *)
  val unsafe_resize : ?round:Fixed_signed.Round.t -> Fixed_signed.t -> Fixed_signed.t

  (** Resize a Fixed_signed.t into this width, with an overflow check. *)
  val safe_resize : ?round:Fixed_signed.Round.t -> Fixed_signed.t -> With_overflow.t

  (** Takes a list of Fixed_signed.t and performs a multiplication operation with [n]
      pipeline registers on the output, normalizing back to the input width and checking
      if there was overflow in the result. *)
  val multiply : n:int -> Signal.Reg_spec.t -> Fixed_signed.t list -> With_overflow.t

  (** Takes a list of Fixed_signed.t and performs an addition operation with [n] pipeline
      registers on the output, normalizing back to the input width and checking if there
      was overflow in the result. *)
  val add : n:int -> Signal.Reg_spec.t -> Fixed_signed.t list -> With_overflow.t

  (** Takes a list of Fixed_signed.t and performs a subtraction operation with [n]
      pipeline registers on the output, normalizing back to the input width and checking
      if there was overflow in the result. *)
  val sub : n:int -> Signal.Reg_spec.t -> Fixed_signed.t list -> With_overflow.t

  module Fixed_signed_as_float : sig
    type nonrec t = Fixed_signed.t [@@deriving sexp_of]
  end

  (** These functions are simulation helpers. *)
  module For_simulation : sig
    (** Converts Bits.t with 32 bits or less in fixed signed format into a float. *)
    val fp_bits_to_float : Bits.t -> float

    (** Converts an arbitrarily large Bits.t into a float. *)
    val large_fp_bits_to_float : Bits.t -> float

    (** Prints Bits.t in a fixed signed format as a float *)
    val print_fp_bits : Bits.t -> unit

    (** Converts a float into a Bits.t in fixed signed format *)
    val float_to_fp_bits : float -> Bits.t

    (** Converts a Bits.t into a fixed signed format *)
    val bits_to_fp : Bits.t -> Fixed_signed.t
  end
end
