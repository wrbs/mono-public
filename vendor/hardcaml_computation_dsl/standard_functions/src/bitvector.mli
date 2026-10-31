(** Operations that treats the values as a bitvector. These nodes require that the input
    be an integer (ie: [f_bits = 0]). They also, by default, require that the inputs are
    unsigned, and require an explicit flag to allow signed inputs. It will raise if those
    requirements are not satisfied. *)

module N = Hardcaml_computation_dsl.Node

val concat_lsb : ?label:string -> N.t list -> N.t

val select
  :  ?label:string
  -> ?allow_signed:bool (** Whether to allow signed inputs. Default [false]. *)
  -> high:int
  -> low:int
  -> N.t
  -> N.t

val drop_top : ?label:string -> ?allow_signed:bool -> width:int -> N.t -> N.t
val sel_top : ?label:string -> ?allow_signed:bool -> width:int -> N.t -> N.t
val drop_bottom : ?label:string -> ?allow_signed:bool -> width:int -> N.t -> N.t
val sel_bottom : ?label:string -> ?allow_signed:bool -> width:int -> N.t -> N.t
val bitwise_not : ?label:string -> ?allow_signed:bool -> N.t -> N.t
val bitwise_or : ?label:string -> ?allow_signed:bool -> N.t -> N.t -> N.t
val bitwise_and : ?label:string -> ?allow_signed:bool -> N.t -> N.t -> N.t
