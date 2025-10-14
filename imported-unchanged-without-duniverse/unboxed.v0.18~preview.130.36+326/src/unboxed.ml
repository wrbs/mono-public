(** [Unboxed] provides canonical aliases for the fundamental unboxed types and their
    corresponding support libraries. This library is intended to be opened unconditionally
    at the top-level in much the same way that {!Core} is. Any module dealing with unboxed
    types in more than a handful of expressions should open this library instead of using
    the builtin type names directly. *)

module F64 = struct
  include Float_u
  module Option = Packed_float_option.Unboxed
end

module UFO = F64.Option
module F32 = Float32_u
module Pct = Percent_u

module I32 = struct
  include Int32_u

  let to_i64 = to_int64_u
  let of_i64_trunc = of_int64_u_trunc
  let of_i64_exn = of_int64_exn
end

module I64 = struct
  include Int64_u

  let of_i32 = I32.to_i64
  let to_i32_trunc = I32.of_i64_trunc
  let to_i32_exn = I32.of_i64_exn
end

module Iptr = struct
  include Nativeint_u

  let[@inline] [@zero_alloc] of_i64_trunc t = I64.box t |> of_int64_trunc
  let[@inline] [@zero_alloc] to_i64 t = to_int64 t |> I64.unbox
  let[@inline] [@zero_alloc] of_i32 t = I32.box t |> of_int32
  let[@inline] [@zero_alloc] to_i32_trunc t = box t |> I32.of_nativeint_trunc
end

type f64 = F64.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type f32 = F32.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type i64 = I64.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type i32 = I32.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type iptr = Iptr.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

let typerep_of_f64 = F64.typerep_of_t
let typerep_of_i64 = I64.typerep_of_t
let typerep_of_i32 = I32.typerep_of_t
let typerep_of_iptr = Iptr.typerep_of_t
