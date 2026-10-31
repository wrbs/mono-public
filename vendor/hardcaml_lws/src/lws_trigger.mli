(** See Lws_core for documentation on the Trigger.t type. *)

type t [@@deriving sexp_of]

val create : created_at:[%call_pos] -> string -> t
val is_equal : t @ local -> t @ local -> bool
