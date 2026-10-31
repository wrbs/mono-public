(** Node constructor for doing fixed-precision inversion with a ROM lookup - given [ x ],
    returns [1 / x ]. *)

module Node := Hardcaml_computation_dsl.Node

val node
  :  ?shared:bool
  -> ?label:string
  -> rom_latency:int
  -> output_f_bits:int
       (** The number of fractional bits in the inverse. Note that this is not necessarily
           the number of fractional bits stored in the ROM - this also depends on the
           number of fractional bits in the input. *)
  -> Node.t
  -> Node.t
