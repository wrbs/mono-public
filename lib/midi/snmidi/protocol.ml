open! Core
open! Async
open Jsonaf.Export

module Seqnum = struct
  type t = Int_repr.Uint32.t [@@deriving sexp, compare, equal]

  let zero = Int_repr.Uint32.zero
  let one = Int_repr.Uint32.of_base_int_exn 1
  let next t = Int_repr.Uint32.O.Wrap.( + ) t one
  let magic_no_check = Int_repr.Uint32.of_base_int_exn 0xDEADBEEF
  let t_of_jsonaf json = Int_repr.Uint32.of_base_int64_exn ([%of_jsonaf: int64] json)
  let jsonaf_of_t t = [%jsonaf_of: int64] (Int_repr.Uint32.to_base_int64 t)
end

module Error_response = struct
  type t =
    { error : string
    ; context : string list [@jsonaf.list] [@sexp.list]
    ; ansi : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Problem = struct
  type t =
    | Server of Error_response.t
    | Client of Error.t
  [@@deriving sexp_of]

  let error_s sexp = Error (Client (Error.create_s sexp))

  let try_with_join f =
    match%map Deferred.Or_error.try_with f ~extract_exn:false with
    | Ok (Ok x) -> Ok x
    | Ok (Error error) -> Error error
    | Error error -> Error (Client error)
  ;;
end

module Message = struct
  type 'a t = ('a, Error_response.t) Result.t [@@deriving sexp_of]

  let jsonaf_of_t inner = function
    | Ok x -> inner x
    | Error error -> [%jsonaf_of: Error_response.t] error
  ;;

  let t_of_jsonaf inner jsonaf =
    match [%of_jsonaf: Error_response.t] jsonaf with
    | error -> Error error
    | exception _ -> Ok (inner jsonaf)
  ;;
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

module Initialize = struct
  module Request = struct
    type t =
      { version : int
      ; client_name : string
      }
    [@@deriving jsonaf, sexp_of]
  end

  module Response = struct
    type t = { ports : Port.t list } [@@deriving jsonaf, sexp_of]
  end

  let desc = "initialize"
end

module Midi_connect = struct
  module Request = struct
    type t = { id : Port.Id.t } [@@deriving jsonaf, sexp_of]
  end

  module Response = struct
    type t = { udp_port : int } [@@deriving jsonaf, sexp_of]
  end

  let desc = "midi connect"
end

module Established_command = struct
  module Json = struct
    module Kind = struct
      type t = Shutdown_without_stop [@@deriving string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    type t = { command : Kind.t } [@@deriving jsonaf]
  end

  type t = Shutdown_without_stop [@@deriving sexp_of]

  let of_jsonafable (wire : Json.t) =
    match wire.command with
    | Shutdown_without_stop -> Shutdown_without_stop
  ;;

  let to_jsonafable : t -> Json.t = function
    | Shutdown_without_stop -> { command = Shutdown_without_stop }
  ;;

  include functor Jsonaf.Jsonafable.Of_jsonafable (Json)
end

module Ack = struct
  type t = { ack : Seqnum.t } [@@deriving jsonaf, sexp_of]
end
