open! Core
open! Hardcaml
module Cpu = Hardnes_rp2a03.Cpu
module Sim = Cyclesim.With_interface (Cpu.I) (Cpu.O)

type t =
  { sim : Sim.t
  ; mutable cyc : int
  }
