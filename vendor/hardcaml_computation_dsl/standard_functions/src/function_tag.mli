open! Core

module Fixed_op : sig
  module Op : sig
    type t =
      | Add
      | Subtract
      | Multiply
      | Div_by_2
      | Inverse_lookup of
          { round_nearest : bool
          (** When converting from [of_float], whether to [round_nearest] or not *)
          }
      | Pipeline
      | Mux
      | Negate
      | Logical_and
      | Logical_or
      | Logical_not
    [@@deriving enumerate, sexp_of, to_string, compare ~localize, hash]
  end

  type t =
    { op : Op.t
    ; latency : int
    }
  [@@deriving sexp_of]
end

module Fixed_compare : sig
  type t =
    | Equals
    | Lt
    | Lte
    | Gt
    | Gte
  [@@deriving enumerate, sexp, to_string]
end

module Resize : sig
  module Is_safe : sig
    type t =
      | Lossless
      | Safe
      | Unsafe
    [@@deriving enumerate, sexp, to_string, compare ~localize, hash]
  end

  module Round : sig
    type t =
      | Neg_infinity
      | Pos_infinity
      | To_zero
      | Away_from_zero
      | Tie_to_neg_infinity
      | Tie_to_pos_infinity
      | Tie_to_zero
      | Tie_away_from_zero
      | Tie_to_nearest_even
      | Tie_to_nearest_odd
    [@@deriving sexp, to_string, compare ~localize, hash]
  end

  module Overflow : sig
    type t =
      | Wrap
      | Saturate
    [@@deriving enumerate, sexp, to_string, compare ~localize, hash]
  end

  type t = Is_safe.t * Round.t option * Overflow.t option
  [@@deriving sexp, compare ~localize, hash]
end

module Shift : sig
  type t =
    | Left
    | Right
    | Right_arithmetic
    | Right_logical
  [@@deriving sexp, to_string, compare ~localize, hash]
end

type t =
  | Divide_by_power
  | Fixed_compare of Fixed_compare.t
  | Fixed_op of Fixed_op.t
  | Resize of Resize.t
  | Shift of Shift.t
  | Onehot_select
  | Priority_select
[@@deriving sexp_of, to_string, compare ~localize, hash]

val of_string : string -> t option
