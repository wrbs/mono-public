open! Core

(** An array of 64 float samples *)

type t [@@deriving sexp_of, equal]

val size : int (* [64] *)
val zero : t
val const : float -> t
val make : (int -> float) -> t
val get : t -> int -> float
val first : t -> float
val last : t -> float

module O : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( +. ) : t -> float -> t
  val ( -. ) : t -> float -> t
  val ( *. ) : t -> float -> t
  val ( /. ) : t -> float -> t
end

include module type of struct
  include O
end

(** maps *)

val map : t -> f:(float -> float) -> t
val mapi : t -> f:(int -> float -> float) -> t
val map2 : t -> t -> f:(float -> float -> float) -> t
val mapi2 : t -> t -> f:(int -> float -> float -> float) -> t
val map3 : t -> t -> t -> f:(float -> float -> float -> float) -> t
val mapi3 : t -> t -> t -> f:(int -> float -> float -> float -> float) -> t

(** folds *)

val fold : t -> init:'acc -> f:('acc -> float -> 'acc) -> 'acc
val foldi : t -> init:'acc -> f:('acc -> int -> float -> 'acc) -> 'acc
val fold_map : t -> init:'acc -> f:('acc -> float -> 'acc * float) -> 'acc * t
val fold_mapi : t -> init:'acc -> f:('acc -> int -> float -> 'acc * float) -> 'acc * t

(** unfolds *)

val unfold : 'state -> f:('state -> 'state * float) -> 'state * t
val unfoldi : 'state -> f:('state -> int -> 'state * float) -> 'state * t
