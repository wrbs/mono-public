open Core
open! Int.Replace_polymorphic_compare

module Stable = struct
  open Core.Core_stable

  module V1 = struct
    (* [Float.compare] compares different nans as equal. We have tests to ensure this. *)
    type t = float
    [@@deriving
      bin_io ~localize, compare ~localize, globalize, sexp, stable_witness, typerep]

    (* We use [Float.abs (0. /. 0.)] for [none] instead of [Float.nan] because the
       compiler currently does not treat Float.nan as a known constant at compile time. In
       some cases, this can lead to unneccessary allocations. The [Float.abs] call ensures
       that when printing the value of [none], we get [nan] and not [-nan] *)
    let none = Core.Float.abs (0. /. 0.)
    let[@zero_alloc] is_none t = Core.Float.is_nan t
    let[@zero_alloc] is_some t = not (is_none t)
    let[@zero_alloc] some_is_representable t = is_some t

    [%%template
    [@@@mode.default m = (global, local)]

    let[@zero_alloc] some (v @ m) =
      assert (is_some v);
      v
    ;;

    let[@zero_alloc] unchecked_value (x : t @ m) = x
    let to_option (t @ m) = if is_none t then None else Some t [@exclave_if_local m]

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> (some [@mode m]) v [@exclave_if_local m]
    ;;]

    let sexp_of_t t = to_option t |> [%sexp_of: float option]
    let t_of_sexp s = [%of_sexp: float option] s |> of_option
    let t_sexp_grammar = [%sexp_grammar: float option] |> Sexplib.Sexp_grammar.coerce
    let equal = [%compare.equal: float]
    let%template equal = [%compare_local.equal: float] [@@mode m = local]
  end
end

include Stable.V1

include%template Comparable.Make_binable [@mode local] (Stable.V1)

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@zero_alloc] is_none t = is_none t

    let%template[@zero_alloc] unsafe_value t =
      (unchecked_value [@mode m]) t [@exclave_if_local m]
    [@@mode m = (global, local)]
    ;;
  end
end

module Array = struct
  include Float_array

  let view_to_float_array_none_as_nan t = t
  let view_of_float_array_nan_as_none t = t
end

open Optional_syntax

[%%template
[@@@alloc a @ m = (heap_global, stack_local)]

let[@zero_alloc] value (t @ m) ~default =
  (match%optional (t : _ @ m) with
   | None -> default
   | Some t -> t)
  [@exclave_if_stack a]
[@@mode m]
;;

let[@zero_alloc] value_exn (t @ m) =
  (match%optional (t : _ @ m) with
   | None -> raise_s [%message "None"]
   | Some t -> t)
  [@exclave_if_stack a]
[@@mode m]
;;]

let value_map t ~f ~default =
  match%optional t with
  | None -> default
  | Some t -> f t
;;

let zero = Float.zero
let[@inline] of_float_nan_as_none (x : float) = x
let to_float_none_as_nan (x : t) = x

module Infix = struct
  (* [nan] behaves essentially correctly for these functions. Note that eg
     [Float.is_nan (0. /. 0.)] *)
  let ( + ) = Float.( + )
  let ( - ) = Float.( - )
  let ( * ) = Float.( * )
  let ( / ) = Float.( / )

  (* We need to check both operands, because:
     Float.nan ** 0. = 1.
     1. ** Float.nan = 1. *)
  let ( ** ) t1 t2 = if is_none t1 || is_none t2 then none else Float.( ** ) t1 t2

  (* These functions return false if either operand is [nan]. *)
  let ( < ) = Float.( < )
  let ( <= ) = Float.( <= )
  let ( > ) = Float.( > )
  let ( >= ) = Float.( >= )

  (* Although [Float.(nan <> nan)] is true, we define [Packed_float_option.(none = none)]
     to be true (per [equal] above). *)
  let ( = ) = equal
  let ( <> ) t1 t2 = not (t1 = t2)
end

module Local = struct
  let globalize = globalize_float
  let of_float_nan_as_none (local_ (x : float)) = exclave_ x
  let to_float_none_as_nan (local_ (x : t)) = exclave_ x

  (* Evaluatues [none = none] to true *)
  let equal = [%compare_local.equal: float]

  module Infix = struct
    let ( + ) = Stdlib.( +. )
    let ( - ) = Stdlib.( -. )
    let ( * ) = Stdlib.( *. )
    let ( / ) = Stdlib.( /. )
    let ( < ) (x : float) y = Stdlib.( < ) x y
    let ( <= ) (x : float) y = Stdlib.( <= ) x y
    let ( > ) (x : float) y = Stdlib.( > ) x y
    let ( >= ) (x : float) y = Stdlib.( >= ) x y
    let ( = ) = equal
    let ( <> ) t1 t2 = not (t1 = t2)
  end

  include Infix

  module Optional_syntax = struct
    module S = Optional_syntax

    module Optional_syntax = struct
      let[@zero_alloc] is_none (local_ (x : t)) = Stdlib.( <> ) x x
      let[@zero_alloc] unsafe_value (local_ (x : t)) = x
    end

    module%test _ = struct
      module L = Optional_syntax

      let%test_unit "is_none" =
        Quickcheck.test
          ~trials:1_000
          ~examples:[ Float.infinity; Float.nan; Float.neg_infinity ]
          ~sexp_of:Float.sexp_of_t
          Float.quickcheck_generator
          ~f:(fun f -> assert (Bool.equal (L.is_none f) (S.is_none f)))
      ;;

      let%test_unit "unsafe_value not nan" =
        (* by definition nan doesn't equal itself so we cannot test it here *)
        Quickcheck.test
          ~trials:1_000
          ~examples:[ Float.infinity; Float.neg_infinity ]
          ~sexp_of:Float.sexp_of_t
          (Quickcheck.Generator.filter
             Float.quickcheck_generator
             ~f:(Fn.non Float.is_nan))
          ~f:(fun f -> assert (Stdlib.( = ) (L.unsafe_value f) (S.unsafe_value f)))
      ;;

      let%test ("unsafe_value nan" [@tags "no-js"]) =
        L.unsafe_value none |> globalize |> Float.is_nan
      ;;
    end
  end

  let is_none = Optional_syntax.Optional_syntax.is_none
end

(* Due to the arithmetic operations provided above, it's possible to get a different
   representation of [None]. For hashing, we make sure to use a canonical [None],
   although it is not necessary at the time, as [Float.hash] returns the same hash for
   all representations of [NaN].

   The performance of keeping vs. removing this branching stays the same,
   proved by benchmarking. *)
let none_hash = Float.hash none

let hash t =
  match%optional t with
  | None -> none_hash
  | Some t -> Float.hash t
;;

let hash_fold_t h t = hash_fold_int h (hash t)
let abs = Float.abs
let neg = Float.neg
let log = Float.log
let log10 = Float.log10
let log1p = Float.log1p
let sqrt = Float.sqrt
let square = Float.square
let exp = Float.exp
let is_inf = Float.is_inf
let is_positive = Float.is_positive
let is_non_positive = Float.is_non_positive
let is_negative = Float.is_negative
let is_non_negative = Float.is_non_negative
let is_integer = Float.is_integer
let is_finite = Float.is_finite
let min = Float.min
let max = Float.max
let clamp = Float.clamp
let clamp_exn = Float.clamp_exn
let inv t = Infix.( / ) (of_float_nan_as_none 1.) t
let scale t flt = Infix.( * ) t (of_float_nan_as_none flt)

let first_some x y =
  match%optional x with
  | Some _ -> x
  | None -> y
;;

let merge x y ~f =
  match%optional x, y with
  | None, None -> none
  | Some x, None -> x
  | None, Some y -> y
  | Some x, Some y -> f x y
;;

let validate ~none:none_check ~some:some_check t =
  match%optional t with
  | None -> none_check ()
  | Some value -> some_check value
;;

let validate_option_bound ~may_be_none ?min:(lower = Unbounded) ?max:(upper = Unbounded) t
  =
  match%optional t with
  | None -> if may_be_none then Validate.pass else Validate.fail "value may not be none"
  | Some value ->
    Validate.bounded ~lower ~upper ~compare:Float.compare ~name:Float.to_string value
;;

let to_string t = Sexp.to_string (sexp_of_t t)

include Infix

module Unboxed = struct
  include (
    Float_u :
    sig
      type t = float#

      val globalize : local_ t -> t

      include%template Bin_prot.Binable.S_any [@mode local] with type t := t

      include Ppx_hash_lib.Hashable.S_any with type t := t

      val typerep_of_t : t Typerep.t
    end)

  let[@inline] [@zero_alloc] equal t1 t2 = [%compare.equal: Float_u.t] t1 t2

  (* We use [compare.equal] here because, the float compare function have the behavior
     that [compare nan nan = 0] (which is not the case for Float.equal). *)
  let[@inline] [@zero_alloc] equal__local (local_ t1) (local_ t2) =
    [%compare.equal: Float_u.t] t1 t2
  ;;

  let[@zero_alloc] none () = Float_u.nan ()
  let[@zero_alloc] is_none (t : t) : bool = Float_u.is_nan (t :> float#)
  let unsafe_value (t : t) : float# = (t :> float#)
  let[@inline] box t = Float_u.to_float t
  let[@inline] box_local (local_ t) = exclave_ Float_u.to_float t
  let[@inline] [@zero_alloc] unbox t = Float_u.of_float t
  let[@inline] [@zero_alloc] unbox_local (local_ t) = Float_u.of_float t
  let[@zero_alloc] is_some t = not (is_none t)
  let[@inline] [@zero_alloc] const t = Float_u.of_float t
  let[@inline] [@zero_alloc] abs t = Float_u.abs t
  let[@inline] [@zero_alloc] select cond t1 t2 = Float_u.select cond t1 t2
  let[@zero_alloc] min t1 t2 = Float_u.min t1 t2
  let[@zero_alloc] max t1 t2 = Float_u.max t1 t2
  let[@inline] [@zero_alloc] unchecked_some v = v
  let[@inline] [@zero_alloc] some_if b v = select b v (none ())

  let[@zero_alloc] some v =
    assert (is_some v);
    unchecked_some v
  ;;

  let of_option_local (local_ opt) =
    match opt with
    | None -> none ()
    | Some x -> some (Float_u.of_float x)
  ;;

  let[@inline] [@zero_alloc] compare t1 t2 = Float_u.compare t1 t2
  let[@inline] [@zero_alloc] first_some x y = Float_u.first_non_nan x y
  let[@inline] [@zero_alloc] some_or x ~default = first_some x (unchecked_some default)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@zero_alloc] is_none t = is_none t
      let[@zero_alloc] unsafe_value t = unsafe_value t
    end
  end

  module Infix = struct
    include Float_u.O

    (* We need to check both operands, because:
       Float.nan ** 0. = 1.
       1. ** Float.nan = 1. *)
    let[@zero_alloc] ( ** ) t1 t2 = if is_none t1 || is_none t2 then none () else t1 ** t2
  end

  module O = Infix

  let merge x y ~f =
    let open Optional_syntax in
    match%optional_u x, y with
    | None, None -> none ()
    | Some x, None -> x
    | None, Some y -> y
    | Some x, Some y -> f x y
  ;;

  let of_option o = of_option o |> unbox

  let to_option t =
    match%optional_u (t : t) with
    | None -> None
    | Some f -> Some (Float_u.to_float f)
  ;;

  let[@cold] raise__no_value (type a : float64) _ : a =
    match raise_s [%message "None"] with
    | (_ : Nothing.t) -> .
  ;;

  let value_exn t =
    match%optional_u (t : t) with
    | None -> raise__no_value t
    | Some f -> f
  ;;

  let to_option_local t = exclave_
    match%optional_u (t : t) with
    | None -> None
    | Some f -> Some (Float_u.to_float f)
  ;;

  let[@inline] [@zero_alloc] neg t = Float_u.neg t
  let[@inline] [@zero_alloc] zero () = Float_u.zero ()
  let[@inline] [@zero_alloc] one () = Float_u.one ()
  let[@inline] [@zero_alloc strict] of_float_nan_as_none (t : float#) : t = t
  let[@inline] [@zero_alloc] to_float_none_as_nan (t : t) : float# = t
  let[@zero_alloc] scale t flt = Infix.( * ) t (of_float_nan_as_none flt)
  let[@zero_alloc] div t flt = Infix.( / ) t (of_float_nan_as_none flt)
  let t_of_sexp sexp = t_of_sexp sexp |> unbox
  let sexp_of_t t = sexp_of_t (box t)
  let to_string t = Sexp.to_string (sexp_of_t t)
  let[@zero_alloc] value t ~default = select (is_some t) (to_float_none_as_nan t) default

  let[@zero_alloc] divide_if_denominator_nonzero_else
    ~(numerator : t)
    ~(denominator : t)
    ~(else_ : t)
    =
    let open Infix in
    let is_denominator_nonzero = denominator <> zero () in
    select is_denominator_nonzero Float_u.(numerator / denominator) else_
  ;;

  include struct
    open Base_quickcheck

    let quickcheck_generator =
      Generator.Via_thunk.map (Generator.option Generator.float) ~f:(fun f () ->
        of_option (f ()))
    ;;

    let quickcheck_observer =
      Observer.Via_thunk.unmap (Observer.option Observer.float) ~f:(fun f () ->
        to_option (f ()))
    ;;

    let quickcheck_shrinker = Shrinker.atomic
  end

  module Array = struct
    include Float_u.Array

    let sexp_of_t = custom_sexp_of_t sexp_of_t
  end

  module Ref = struct
    include Float_u.Ref

    let[@zero_alloc] create_local contents = exclave_ { contents }
    let[@inline] create_none () = create (none ())
    let[@inline] [@zero_alloc] set_none t = set t (none ())
  end

  module Stable = struct
    module V1 = struct
      module F = Float_u.Stable.V1

      type t = F.t [@@deriving globalize, stable_witness]

      include (
        F :
        sig
          include%template Bin_prot.Binable.S_any [@mode local] with type t := t

          include Ppx_hash_lib.Hashable.S_any with type t := t

          val typerep_of_t : t Typerep.t
          val equal : t -> t -> bool
          val compare : t -> t -> int
        end)

      type sexp_repr = float option [@@deriving sexp]

      let sexp_of_t t = to_option t |> [%sexp_of: sexp_repr]
      let t_of_sexp s = [%of_sexp: sexp_repr] s |> of_option
    end
  end
end

let[@inline] divide_if_denominator_nonzero_else
  ~(numerator : t)
  ~(denominator : t)
  ~(else_ : t)
  =
  Unboxed.divide_if_denominator_nonzero_else
    ~numerator:(Unboxed.unbox numerator)
    ~denominator:(Unboxed.unbox denominator)
    ~else_:(Unboxed.unbox else_)
  |> Unboxed.box
;;
