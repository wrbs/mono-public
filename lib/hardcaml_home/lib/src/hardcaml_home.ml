(* Extension of basic stuff *)
module Comb_extended = Comb_extended0.Make
module Bits_extended = Comb_extended0.Bits_extended
module Signal_extended = Comb_extended0.Signal_extended

(* Other modules *)
module Dynamic_interface = Dynamic_interface
module Component = Component
module Crc = Crc
module Validated = Validated

(* rest of hardcaml *)
include Hardcaml
