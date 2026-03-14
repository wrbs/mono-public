open! Core
open! Hardcaml

module type S = sig
  include Interface.Pre_partial

  type bits_param

  val port_names_and_widths_dynamic : nbits:bits_param -> (string * int) t
  val wave_formats : Wave_format.t t

  module Of_signal : sig
    type nonrec t = Signal.t t

    val wires : bits_param -> t
    val ( -- ) : t -> string -> t
    val __ppx_auto_name : t -> string -> t
  end
end

module type Dynamic_interface = sig
  module type S = S

  module To_interface
      (T : S)
      (_ : sig
         val bits : T.bits_param
       end) : Interface.S with type 'a t = 'a T.t

  (** Most generic way to use with [[@deriving hardcaml ~pre]] *)
  module Make_of_interface'
      (Bits_param : T)
      (T : Interface.Pre)
      (_ : sig
         val override_widths : Bits_param.t -> int option T.t
         val wave_formats : [ `Default | `Custom of Wave_format.t T.t ]
       end) : S with type 'a t = 'a T.t and type bits_param = Bits_param.t

  (** Custom helper: bits -1 = nbits, -2 = nbits * 2, etc.

      Designed for use with include functor *)
  module Of_deriving_hardcaml_pre (T : Interface.Pre) :
    S with type 'a t := 'a T.t and type bits_param = int
end
