open! Core
open! Hardcaml

(** Represents a stream of _recieved_ packets. can handle 0-length packets.

    Steps:

    - start = 1 (will happen on its own cycle: indicates that at any time after the
      current cycle there can/will be data)
    - data as many times as needed
      - if receiving from wire: valid set when data ready, consumer must always be ready
      - if sending to wire: ready set when when upstream is ready, producer must always
        have date after setting start
    - stop sent on last byte or after it (when sending only when ready)
    - abort = whenever set, reset yourself/mess up crcs on send

    start indicates other per-packet data is valid and stable at least until stop *)

type 'a t =
  { data : 'a
  ; start : 'a
  ; stop : 'a
  ; abort : 'a
  }
[@@deriving hardcaml]

module Always_helper : sig
  type t =
    { ensure_started : Always.t
    ; stop_if_started : Always.t
    ; emit : Signal.t -> Always.t
    ; abort : Always.t
    ; reset : Always.t
    ; abort_no_reset : Always.t
    }

  val compile_with
    :  Scope.t
    -> reg_spec:Signal.Reg_spec.t
    -> upstream_abort:Signal.t
    -> reset:Always.t list
    -> (t -> Always.t list)
    -> Of_signal.t * valid:Signal.t
end

(** Buffers pending starts until [ready_for_start] passed *)
val start_buffer
  :  Scope.t
  -> Signal.t t
  -> ready_to_start:Signal.t
  -> ready_for_data:Signal.t
  -> reg_spec:Signal.Reg_spec.t
  -> Signal.t t * emitting:Signal.t

(** Gates incoming packets *)
val gate
  :  Scope.t
  -> Signal.t t
  -> start_valid:Signal.t
  -> reg_spec:Signal.Reg_spec.t
  -> Signal.t t * running:Signal.t
