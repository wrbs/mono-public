open Hardcaml

module type Arg = sig
  (** Row and height of the systolic array *)
  val n : int

  val element_width : int

  (** When true, input_[{top,left}].(i) has a register delaying it by `i` clock cycles. *)
  val delay_inputs : bool

  val instantiate_processing_element
    :  clocking:Signal.t Clocking.t
    -> enable:Signal.t
    -> left:Signal.t
    -> top:Signal.t
    -> Signal.t * right:Signal.t * bottom:Signal.t
end

module type Systolic_array = sig
  module Make (M : Arg) : sig
    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; input_top : 'a array
        (** Size [n] vector corresponding to the top of the systolic array. top.(0) is the
            left most element. *)
        ; input_left : 'a array
        (** Size [n] vector corresponding to the left of the systolic array. left.(0) is
            the top most element. *)
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { processing_element_contents : 'a array
        (** A [n * n] array with the contents of the systolic array. *)
        }
      [@@deriving hardcaml]
    end

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
