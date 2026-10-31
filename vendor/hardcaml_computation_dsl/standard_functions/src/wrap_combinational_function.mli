open! Core
open Hardcaml
open Hardcaml_computation_dsl

module Make_three_input_one_output
    (In1 : Interface_with_precision.S)
    (In2 : Interface_with_precision.S)
    (In3 : Interface_with_precision.S)
    (Out : Interface_with_precision.S) : sig
  val wrap_function__unsafe
    :  tag:string
    -> latency:int
    -> (Scope.t
        -> clocking:Signal.t Clocking.t
        -> build_mode:Build_mode.t
        -> Signal.t In1.t
        -> Signal.t In2.t
        -> Signal.t In3.t
        -> Signal.t Out.t)
    -> Node.t In1.t
    -> Node.t In2.t
    -> Node.t In3.t
    -> Node.t Out.t

  val wrap_combinational_function
    :  tag:string
    -> (Signal.t In1.t -> Signal.t In2.t -> Signal.t In3.t -> Signal.t Out.t)
    -> Node.t In1.t
    -> Node.t In2.t
    -> Node.t In3.t
    -> Node.t Out.t
end

module Make_two_input_one_output
    (In1 : Interface_with_precision.S)
    (In2 : Interface_with_precision.S)
    (Out : Interface_with_precision.S) : sig
  val wrap_combinational_function
    :  tag:string
    -> (Signal.t In1.t -> Signal.t In2.t -> Signal.t Out.t)
    -> Node.t In1.t
    -> Node.t In2.t
    -> Node.t Out.t
end

module Make_one_input_one_output
    (In : Interface_with_precision.S)
    (Out : Interface_with_precision.S) : sig
  val wrap_combinational_function
    :  tag:string
    -> (Signal.t In.t -> Signal.t Out.t)
    -> Node.t In.t
    -> Node.t Out.t
end

module Make_one_input_signal_output (In : Interface_with_precision.S) : sig
  val wrap_combinational_function
    :  tag:string
    -> output_precision:Precision.t
    -> (Signal.t In.t -> Signal.t)
    -> Node.t In.t
    -> Node.t
end

module Make_signal_input_one_output (Out : Interface_with_precision.S) : sig
  val wrap_function__unsafe
    :  input_precision:Precision.t
    -> tag:string
    -> latency:int
    -> (Scope.t
        -> clocking:Signal.t Clocking.t
        -> build_mode:Build_mode.t
        -> Signal.t
        -> Signal.t Out.t)
    -> Node.t
    -> Node.t Out.t

  val wrap_combinational_function
    :  input_precision:Precision.t
    -> tag:string
    -> (Signal.t -> Signal.t Out.t)
    -> Node.t
    -> Node.t Out.t
end
