open Hardcaml
module Clocking = Types.Clocking
include Computation_interface_intf

module Make_I (Dynamic_inputs : Interface.S) (Static_inputs : Interface.S) = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; dynamic_inputs : 'a Dynamic_inputs.t [@rtlprefix "dynamic_inputs$"]
    ; static_inputs : 'a Static_inputs.t [@rtlprefix "static_inputs$"]
    }
  [@@deriving hardcaml ~rtlmangle:false]
end

module Make_O (Output : Interface.S) = struct
  type 'a t =
    { overflow_cnt : 'a [@bits 32]
    ; q : 'a Output.t
    ; overflow : 'a
    ; valid : 'a
    ; ready : 'a
    }
  [@@deriving hardcaml]
end
