(** This interface provides common I/O interfaces for hierarchical computation pipelines. *)

open Hardcaml
module Interface_with_precision = Hardcaml_computation_dsl.Interface_with_precision

(** Construct the input interface for a given computation pipeline. *)
module MI (Dynamic_inputs : Interface.S) (Static_inputs : Interface.S) = struct
  module type S = sig
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; start : 'a
      ; dynamic_inputs : 'a Dynamic_inputs.t
      ; static_inputs : 'a Static_inputs.t
      }
    [@@deriving hardcaml]
  end
end

(** The output interface for any computation pipeline. *)
module MO (Output : Interface.S) = struct
  module type S = sig
    type 'a t =
      { overflow_cnt : 'a
      ; q : 'a Output.t
      ; overflow : 'a
      ; valid : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end
end

(** The module type of any computation. Computations that implement this interface can be
    plugged into other tooling provided in this library (i.e. automated quickchecks). End
    users will never have to do this themselves - there are pipeline generators provided
    in this library to generate computation pipeline modules with this interface (i.e.
    [fully_pipelined.ml]). *)
module type S = sig
  module Dynamic_inputs : Interface_with_precision.S
  module Static_inputs : Interface_with_precision.S
  module Output : Interface_with_precision.S
  module I : MI(Dynamic_inputs)(Static_inputs).S
  module O : MO(Output).S

  val latency : int

  val hierarchical
    :  ?only_clear_counters:bool (** See [static_intf.ml] for details. *)
    -> ?instance:string
    -> Scope.t
    -> build_mode:Build_mode.t
    -> Interface.Create_fn(I)(O).t
end

module type Computation_interface = sig
  module type S = S

  module MI = MI
  module MO = MO
  module Make_O (Output : Interface.S) : MO(Output).S

  module Make_I (Dynamic_inputs : Interface.S) (Static_inputs : Interface.S) :
    MI(Dynamic_inputs)(Static_inputs).S
end
