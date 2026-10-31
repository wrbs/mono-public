(** Module to implement a fully pipelined 1D convolution.

    The kernel is required to be held stable throughout the usage of this module. The
    input elements into the convolution block is given one element at a time, gated with
    the [input_element.valid] signal. *)

open Hardcaml
module Precision = Hardcaml_computation_dsl.Precision

module Make (Config : Config.S) : sig
  val element_precision : Precision.t

  (** The number of bits an element occupies. *)
  val element_width : int

  (** The latency from [input_element.valid] and the corresponding result
      [output_element.valid] being asserted. *)
  val latency : int

  (** Interface for kernel weights - runtime configurable *)
  module Kernel : sig
    type 'a t = { weights : 'a list } [@@deriving hardcaml]
  end

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; kernel : 'a Kernel.t
      ; input_element : 'a With_valid.t
      ; clear_sliding_window : 'a
      (** When [clear_sliding_window] is pulsed, the sliding window is cleared to zeros *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { output_element : 'a With_valid.t } [@@deriving hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
