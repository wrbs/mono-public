(** Looks up the inverse of an integer to a given fixed precision. Can be used to
    implement arbitrary integer division (as fixed-precision multiplication by the
    inverse).

    Assumes unsigned inputs, but outputs a signed value (always nonnegative). *)

open Hardcaml

module type Config = sig
  val rom_round_nearest : bool
  val rom_latency : int
  val input_i_bits : int
  val output_f_bits : int
end

(** Hardcoded number of integer bits in the inverted value. *)
val output_i_bits : int

module Make (Config : Config) : sig
  module Fixed_utils_rom_config : sig
    val i_bits : int
    val f_bits : int
  end

  module Fixed_utils_rom : module type of Fixed_prims.Make (Fixed_utils_rom_config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; a : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { q : 'a
      ; error : 'a (** Whether the input was 0. *)
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
