(** Implementation of variable shift operations to multiply and divide by powers of 2. *)

module Node := Hardcaml_computation_dsl.Node

(** Left shift. *)
val sll : ?label:string -> Node.t -> Node.t -> Node.t

(** Right shift (infers the appropriate behavior based on whether the input is signed or
    not. *)
val sr : ?label:string -> Node.t -> Node.t -> Node.t

(** Logical right shift (shifts in zeros). *)
val srl : ?label:string -> Node.t -> Node.t -> Node.t

(** Arithmetic right shift (preserves sign). *)
val sra : ?label:string -> Node.t -> Node.t -> Node.t
