(** Low-level driver to interact with the convolution demo design. *)

open Core

module Make (Hardware_io : Hardware_io.S) : sig
  module Handler := Hardware_io.Handler

  type t

  val create : Handler.t @ local -> Hardware_io.t -> t

  val reset_kernel_window
    :  Handler.t @ local
    -> t
    -> kernel:float# array
    -> unit Or_error.t

  val convolution_window_size : t -> int

  val write_inputs_and_read_outputs
    :  Handler.t @ local
    -> t
    -> src:float# array
    -> dst:float# array
    -> unit
end
