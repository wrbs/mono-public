(** Hardware_io.S is an abstraction over the low-level interface to interact with a
    hardware design.

    There is two main type of communication:

    - Register IO - these are internal bus interfaces on the top-level design.
    - DMA - these are AXI stream interfaces on the top-level design. *)

open Core

module type S = sig
  module Handler : T

  type t

  val read_register : Handler.t @ local -> t -> byte_address:int -> int
  val write_register : Handler.t @ local -> t -> byte_address:int -> int -> unit
  val host_to_fpga : Handler.t @ local -> t -> Bigstring.t @ local -> len:int -> unit
  val fpga_to_host : Handler.t @ local -> t -> Bigstring.t @ local -> int
end

module type Hardware_io = sig
  module type S = S
end
