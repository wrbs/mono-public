(** Node constructor for a divide-by-power module, used to divide a variable integer power
    from the given [dividend]. See documentation in [divide_by_node.ml]. *)

module Node := Hardcaml_computation_dsl.Node

val node
  :  ?shared:bool
  -> ?label:string
  -> base:int
  -> pow_base_bounds:Divide_by_power.Inclusive_integer_range.t
  -> output_i_bits:int
  -> rom_latency:int
  -> multiply_latency:int
  -> check_for_error:bool
  -> dividend:Node.t
  -> pow:Node.t
  -> unit
  -> Node.t

(** Node to verify that a division done by the above node was valid - i.e. the input
    [dividend] is actually an integer multiple of [base ** pow] and didn't overflow the
    output precision. See
    [Hardcaml_computation_dsl_generators_test.Divide_by_power_and_verify] for an example
    usage. *)
val verify
  :  ?shared:bool
  -> ?label:string
  -> base:int
  -> pow_base_bounds:Divide_by_power.Inclusive_integer_range.t
  -> input_i_bits:int
  -> rom_latency:int
  -> multiply_latency:int
  -> dividend:Node.t
  -> quotient:Node.t
  -> pow:Node.t
  -> unit
  -> Node.t

val is_divisible
  :  ?shared:bool
  -> ?label:string
  -> base:int
  -> pow_base_bounds:Divide_by_power.Inclusive_integer_range.t
  -> input_i_bits:int
  -> rom_latency:int
  -> multiply_latency:int
  -> dividend:Node.t
  -> pow:Node.t
  -> unit
  -> Node.t
