(** A basic example fully-pipelined computation implemented using the
    [Hardcaml_computation_dsl_backends.Fully_pipelined] generator. Used to write tests
    against. *)

open Hardcaml_computation_dsl

module Dynamic_inputs : sig
  type 'a t =
    { base : 'a
    ; input0 : 'a
    ; input1 : 'a
    ; input2 : 'a
    ; input3 : 'a
    ; input6 : 'a
    }
  [@@deriving hardcaml, compare]

  val precision : Precision.t t
end

module Static_inputs : sig
  type 'a t =
    { scale0 : 'a
    ; scale1 : 'a
    ; offset1 : 'a
    ; shift2 : 'a
    ; shift3 : 'a
    }
  [@@deriving hardcaml, compare]

  val precision : Precision.t t
end

module Output : sig
  type 'a t =
    { q1 : 'a
    ; q2 : 'a
    ; q3 : 'a
    ; q4 : 'a
    ; q5 : 'a
    ; q6 : 'a
    ; q7 : 'a
    ; q8 : 'a
    ; q9 : 'a
    ; q10 : 'a
    ; q11 : 'a
    ; q12 : 'a
    ; q13_valid : 'a
    ; q13_value : 'a
    ; q14 : 'a
    ; q15 : 'a
    ; q16 : 'a
    ; q17 : 'a
    ; q18 : 'a
    ; q19 : 'a
    }
  [@@deriving hardcaml, compare]

  val precision : Precision.t t
end

include
  Tester.To_test
  with module Dynamic_inputs := Dynamic_inputs
   and module Static_inputs := Static_inputs
   and module Output := Output
