open! Core
open! Hardcaml_home

module Hw : sig
  module Rx : sig
    type 'a t =
      { crsdv : 'a
      ; rxerr : 'a
      ; rxd : 'a
      }
    [@@deriving hardcaml]
  end

  module Tx : sig
    type 'a t =
      { txen : 'a
      ; txd : 'a [@bits 2] [@wave_format Binary]
      }
    [@@deriving hardcaml]
  end
end

module Rx : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; hw : 'a Hw.Rx.t
      ; ignore_crc : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { rx : 'a Packet_stream.t
      ; valid : 'a
      ; drop : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end

module Tx_ll : sig
  (** Low level driver handling CRCs but nothing else

      Semantics:

      - when [ready_to_start], pulse [start] to start sending the packet (preamble, sfd,
        ...)
      - then when [ready_for_data], provide data over [data]
      - at any point after the cycle [start] was high, use [stop_no_crc] to immediately
        stop things
      - only when [ready_for_data], stop_bad_c
      - if the sfd has not yet been emitted ([sfd_sent] low), all the [stop]s have
        identical semantics (stop transmitting, wait IPG. If they have, they work
        differently) *)
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; data : 'a [@bits 8]
      ; start : 'a (** pulse high to start packet *)
      ; stop_abort : 'a
      (** pulse high to stop without crc: always can be sent after [start] *)
      ; stop_with_crc : 'a
      (** pulse high to stop with crc. if [stop_abort] also set, that crc will be spoiled: *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { hw : 'a Hw.Tx.t
      ; ready_to_start : 'a (** Calling [start] does something *)
      ; ready_for_data : 'a (** [data] read *)
      ; sfd_sent : 'a (** now sending data (stops after [stop] called) *)
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end

module Tx : sig
  (** Higher level driver (uses tx_ll with stream semantics) *)
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx : 'a Packet_stream.t
      ; last_header_byte :
          'a (* pulse when sending the last ethernet header byte, enables crc *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { hw : 'a Hw.Tx.t
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end
