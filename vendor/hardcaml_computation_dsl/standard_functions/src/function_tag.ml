open! Core

module Fixed_op = struct
  module Op = struct
    type t =
      | Add
      | Subtract
      | Multiply
      | Div_by_2
      | Inverse_lookup of { round_nearest : bool }
      | Pipeline
      | Mux
      | Negate
      | Logical_and
      | Logical_or
      | Logical_not
    [@@deriving enumerate, sexp, compare ~localize, hash]

    let to_string = function
      | Add -> "add"
      | Subtract -> "subtract"
      | Multiply -> "multiply"
      | Div_by_2 -> "div_by_2"
      | Inverse_lookup _ -> "inverse_lookup"
      | Pipeline -> "pipeline"
      | Mux -> "mux"
      | Negate -> "negate"
      | Logical_and -> "logical_and"
      | Logical_or -> "logical_or"
      | Logical_not -> "logical_not"
    ;;
  end

  type t =
    { op : Op.t
    ; latency : int
    }
  [@@deriving sexp, compare ~localize, hash]
end

module Fixed_compare = struct
  type t =
    | Equals
    | Lt
    | Lte
    | Gt
    | Gte
  [@@deriving
    enumerate, sexp, to_string ~capitalize:"snake_case", compare ~localize, hash]
end

module Resize = struct
  module Is_safe = struct
    type t =
      | Lossless
      | Safe
      | Unsafe
    [@@deriving
      enumerate, to_string ~capitalize:"snake_case", sexp, compare ~localize, hash]
  end

  module Round = struct
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
    [@@deriving to_string ~capitalize:"snake_case", sexp, compare ~localize, hash]
  end

  module Overflow = struct
    type t =
      | Wrap
      | Saturate
    [@@deriving
      enumerate, to_string ~capitalize:"snake_case", sexp, compare ~localize, hash]
  end

  type t = Is_safe.t * Round.t option * Overflow.t option
  [@@deriving sexp, compare ~localize, hash]
end

module Shift = struct
  type t =
    | Left
    | Right
    | Right_arithmetic
    | Right_logical
  [@@deriving to_string ~capitalize:"snake_case", sexp, compare ~localize, hash]
end

type t =
  | Divide_by_power
  | Fixed_compare of Fixed_compare.t
  | Fixed_op of Fixed_op.t
  | Resize of Resize.t
  | Shift of Shift.t
  | Onehot_select
  | Priority_select
[@@deriving sexp, compare ~localize, hash]

let to_string t = sexp_of_t t |> Sexp.to_string

let of_string s =
  match Sexp.of_string_conv s t_of_sexp with
  | `Result t -> Some t
  | `Error _ -> None
;;
