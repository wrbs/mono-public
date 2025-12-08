open! Core
open! Async

module Seqnum : sig
  type t = Int_repr.Uint32.t [@@deriving sexp, compare, equal]

  val zero : t
  val next : t -> t
  val magic_no_check : t
end

module Error_response : sig
  type t =
    { error : string
    ; context : string list
    ; ansi : string option
    }
  [@@deriving jsonaf, sexp_of]
end

module Problem : sig
  (* main mli exposes more helpers *)
  type t =
    | Server of Error_response.t
    | Client of Error.t
  [@@deriving sexp_of]

  val error_s : Sexp.t -> (_, t) result
  val try_with_join : (unit -> ('a, t) result Deferred.t) -> ('a, t) result Deferred.t
end

module Message : sig
  type 'a t = ('a, Error_response.t) result [@@deriving jsonaf, sexp_of]
end

module type Call = sig
  module Request : sig
    type t [@@deriving jsonaf_of, sexp_of]
  end

  module Response : sig
    type t [@@deriving of_jsonaf, sexp_of]
  end

  val desc : string
end

module Initialize : sig
  module Request : sig
    type t =
      { version : int
      ; client_name : string
      }
    [@@deriving jsonaf, sexp_of]
  end

  module Response : sig
    type t = { ports : Port.t list } [@@deriving jsonaf, sexp_of]
  end

  include Call with module Request := Request and module Response := Response
end

module Midi_connect : sig
  module Request : sig
    type t = { id : Port.Id.t } [@@deriving jsonaf, sexp_of]
  end

  module Response : sig
    type t = { udp_port : int } [@@deriving jsonaf, sexp_of]
  end

  include Call with module Request := Request and module Response := Response
end

module Established_command : sig
  type t = Shutdown_without_stop [@@deriving jsonaf, sexp_of]
end

module Ack : sig
  type t = { ack : Seqnum.t } [@@deriving jsonaf, sexp_of]
end
