open! Core
open! Hardcaml

module type Stream_t = sig
  type 'a t =
    { data : 'a
    ; valid : 'a
    ; error : 'a
    ; last : 'a
    }

  include Dynamic_interface.S with type 'a t := 'a t and type bits_param = int
end

module Definitions (Stream : Stream_t) = struct
  module type S = sig
    type 'a t = 'a Stream.t =
      { data : 'a
      ; valid : 'a
      ; error : 'a
      ; last : 'a
      }
    [@@deriving hardcaml]

    module Source : sig
      type t = tx_ready:Signal.t -> error:Signal.t -> Signal.t Stream.t

      val cat : t list -> clocking:Signal.t Clocking.t -> t
      val append_empty : t -> t -> is_empty:Signal.t -> clocking:Signal.t Clocking.t -> t
    end

    module Header_generator (Header : Interface.S) : sig
      val create
        :  ?valid:Signal.t Header.t
        -> Scope.t
        -> clocking:Signal.t Clocking.t
        -> header:Signal.t Header.t
        -> Source.t
    end

    module Header_parser
        (Header : Interface.S)
        (_ : sig
           val kind : string
         end) : sig
      module Ready : Interface.S with type 'a t = 'a Header.t

      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; stream : 'a Stream.t
          }
        [@@deriving hardcaml]
      end

      module O : sig
        type 'a t =
          { data : 'a Header.t
          ; ready : 'a Ready.t
          ; finished : 'a
          ; error : 'a
          ; next : 'a Stream.t
          }
        [@@deriving hardcaml]
      end

      val create : Scope.t -> Signal.t I.t -> Signal.t O.t
      val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
    end
  end
end

module type Stream = sig
  include Stream_t
  include functor functor (Stream : Stream_t) -> module type of Definitions (Stream)

  module Make (_ : sig
      val bits : int
    end) : S

  module Byte : S
end
