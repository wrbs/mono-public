open! Core

module Effect : sig
  type 'a t = 'a Ui_effect.t

  val sequence : unit t list -> unit t
  val no_op : unit t
  val external_ : string -> unit t
end

module Incr = Ui_incr

include module type of struct
  include Expect_test_helpers_core
end

val dummy_source_code_position : Source_code_position.t
val opaque_const : 'a -> 'a Bonsai.Computation.t
val opaque_const_value : 'a -> 'a Bonsai.Value.t
val opaque_computation : 'a Bonsai.Computation.t -> 'a Bonsai.Computation.t
