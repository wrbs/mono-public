open Core
open Hardcaml
open Hardcaml_computation_dsl
open Function

module Compare (C : sig
    val op : Function_tag.Fixed_compare.t
  end) =
struct
  include C

  let tag =
    let open Function_tag in
    Fixed_compare op |> to_string
  ;;

  let fixed_compare_is_signed = function
    | [ (p0 : Precision.t); p1 ] -> p0.signed || p1.signed
    | _ as input_precisions ->
      raise_s
        [%message
          "[Fixed_compare] expects two inputs." (input_precisions : Precision.t list)]
  ;;

  let infer_output_precision _ = Precision.bool

  let create _scope ~clocking:_ ~build_mode:_ ~input_precisions (inputs : Input.t) =
    let compare_is_signed = fixed_compare_is_signed input_precisions in
    assert (List.length inputs = List.length input_precisions);
    let module Fixed_point =
      (val if compare_is_signed
           then
             (module Hardcaml_fixed_point.Signed (Signal)
             : Hardcaml_fixed_point.Fixed_point
               with type bits = Signal.t)
           else (module Hardcaml_fixed_point.Unsigned (Signal)))
    in
    let op =
      match op with
      | Equals -> Fixed_point.( ==: )
      | Lt -> Fixed_point.( <: )
      | Lte -> Fixed_point.( <=: )
      | Gt -> Fixed_point.( >: )
      | Gte -> Fixed_point.( >=: )
    in
    let inputs =
      List.map2_exn inputs input_precisions ~f:(fun input input_precision ->
        (* Instead of resizing based on [compare_precision], we just let [Fixed_point]
           handle this internally. All we need to do is ensure that converting an unsigned
           input into a signed input is done correctly. *)
        let extended_input =
          if compare_is_signed && not input_precision.signed
          then Signal.ue input
          else input
        in
        Fixed_point.create input_precision.f_bits extended_input)
    in
    { Data.q = op (List.nth_exn inputs 0) (List.nth_exn inputs 1); overflow = Signal.gnd }
  ;;
end

module Equals = Compare (struct
    let op = Function_tag.Fixed_compare.Equals
  end)

module Lt = Compare (struct
    let op = Function_tag.Fixed_compare.Lt
  end)

module Lte = Compare (struct
    let op = Function_tag.Fixed_compare.Lte
  end)

module Gt = Compare (struct
    let op = Function_tag.Fixed_compare.Gt
  end)

module Gte = Compare (struct
    let op = Function_tag.Fixed_compare.Gte
  end)

module type Arg = sig
  val infer_output_precision : Precision.t list -> Precision.t
  val tag : string

  val create
    :  Scope.t
    -> clocking:Signal.t Clocking.t
    -> build_mode:Build_mode.t
    -> input_precisions:Precision.t list
    -> Input.t
    -> Signal.t Data.t
end

let node (module M : Arg) ?(shared = false) ?label a b =
  let open M in
  Node.generic_function_single
    ?label
    ~tag
    ~latency:0
    ~infer_output_precision
    ~create
    ~propagate_input_overflows:None
    ~shared
    [ a; b ]
;;

let equals = node (module Equals)
let lt = node (module Lt)
let lte = node (module Lte)
let gt = node (module Gt)
let gte = node (module Gte)

let is_zero arg =
  let zero =
    let precision = Node.precision arg in
    Node.constant ~precision ~label:"zero" ~value:0 ()
  in
  equals arg zero
;;

let is_not_zero arg = Fixed_op.logical_not (is_zero arg)

let greater_than_zero arg =
  let zero =
    let precision = Node.precision arg in
    Node.constant ~precision ~label:"zero" ~value:0 ()
  in
  gt arg zero
;;

let abs x =
  let x_precision = Node.precision x in
  [%test_result: bool]
    x_precision.signed
    ~expect:true
    ~message:"Don't take abs() of an unsigned value.";
  let negative_x = Fixed_op.negate x in
  let padded_x =
    let negative_x_precision = Node.precision negative_x in
    assert (
      Precision.equal
        negative_x_precision
        { x_precision with i_bits = x_precision.i_bits + 1 });
    (* This is always safe because [negative_x] will always have a wider precision than
       [x], as asserted above. *)
    Resize.unsafe ~precision:negative_x_precision x
  in
  let res = Fixed_op.mux2 (greater_than_zero x) padded_x negative_x in
  (* We can cast this to be unsigned unsafely because we know the output will always be
     unsigned.
  *)
  Resize.unsafe ~precision:{ x_precision with signed = false } res
;;

let max a b =
  let a_greater_than_b = gt a b in
  Fixed_op.mux2 a_greater_than_b a b
;;
