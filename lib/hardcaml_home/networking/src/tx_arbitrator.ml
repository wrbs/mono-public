open! Core
open! Hardcaml_home
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; ready : 'a
    ; tx_a : 'a Packet_stream.t
    ; tx_b : 'a Packet_stream.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { tx : 'a Packet_stream.t
    ; ready_a : 'a
    ; ready_b : 'a
    ; b_emitting : 'a
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let reg_spec = Clocking.to_spec i.clocking in
  let%hw b_emitting = wire 1 in
  let a_buffered, ~emitting:a_emitting =
    Packet_stream.start_buffer
      (Scope.sub_scope scope "a")
      i.tx_a
      ~ready_to_start:~:b_emitting
      ~ready_for_data:i.ready
      ~reg_spec
  in
  let b_buffered, ~emitting:b_emitting' =
    Packet_stream.start_buffer
      (Scope.sub_scope scope "a")
      i.tx_a
      ~ready_to_start:(~:a_emitting &: ~:(a_buffered.start))
      ~ready_for_data:i.ready
      ~reg_spec
  in
  assign b_emitting b_emitting';
  let%hw ready_a = a_emitting &: i.ready in
  let%hw ready_b = b_emitting &: i.ready in
  let%hw.Packet_stream.Of_signal tx =
    Packet_stream.Of_signal.mux b_emitting [ a_buffered; b_buffered ]
  in
  { tx; ready_a; ready_b; b_emitting }
;;

let name = "tx_arbitrator"

include functor Component.Make
