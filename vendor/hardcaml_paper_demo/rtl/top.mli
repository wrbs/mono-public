(** Top-level for implementing 1D convolution. *)

open Hardcaml

(** 32-bit internal bus for register read / write access *)
module Internal_bus = Hardcaml_axi.Internal_bus.W32

(** Streaming interface for DMA packets between the host and FPGA *)
module Stream : Hardcaml_axi.Stream.S

(** Functor to create Top module with a specific configuration *)
module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; host_to_fpga : 'a Stream.Source.t
      ; fpga_to_host_dest : 'a Stream.Dest.t
      ; reg_master : 'a Internal_bus.Master_to_slave.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { host_to_fpga_dest : 'a Stream.Dest.t
      ; fpga_to_host : 'a Stream.Source.t
      ; reg_slave : 'a Internal_bus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
