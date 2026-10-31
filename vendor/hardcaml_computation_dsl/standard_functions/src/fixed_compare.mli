(** Default node constructors for basic fixed-precision comparators. *)

module Node := Hardcaml_computation_dsl.Node

val equals : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val lt : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val lte : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val gt : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val gte : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t

(** {2 Syntactic sugar} *)

val is_zero : Node.t -> Node.t
val is_not_zero : Node.t -> Node.t
val greater_than_zero : Node.t -> Node.t
val abs : Node.t -> Node.t
val max : Node.t -> Node.t -> Node.t
