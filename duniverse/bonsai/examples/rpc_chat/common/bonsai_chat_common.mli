open! Core
module Room : String_id.S

module Message : sig
  type t =
    { room : Room.t
    ; author : string
    ; contents : string
    }
  [@@deriving sexp, bin_io, fields ~getters]
end

module Protocol : sig
  open! Async_rpc_kernel

  module Message_stream : sig
    val t : (unit, Message.t, unit) Rpc.Pipe_rpc.t
  end

  module Messages_request : sig
    val t : (Room.t, Message.t list) Rpc.Rpc.t
  end

  module Send_message : sig
    val t : (Message.t, unit Or_error.t) Rpc.Rpc.t
  end

  module Create_room : sig
    val t : (Room.t, unit Or_error.t) Rpc.Rpc.t
  end

  module List_rooms : sig
    val t : (unit, Room.t list) Rpc.Rpc.t
  end
end
