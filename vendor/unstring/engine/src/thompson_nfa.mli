(** Thompson's NFA (nondeterministic finite automaton with epsilon transitions).

    Thompson's NFA is built from the provided grammar using the standard Thompson
    construction, associating type parameters with each entity.

    - ['a Node.t] is the node type; ['a] denotes the shape of the result stack
      (['a Heterogeneous_list.t]) expected at this node
    - ['a Token_edge.t] is the type for an edge associated with a token transition. It
      connects one ['a Node.t] to another ['a Node.t]; the token transition does not
      affect the result stack.
    - [('src, 'dst) Eps_edge.t] is the type for an edge associated with an epsilon
      transition. It connects a ['src Node.t] to a ['dst Node.t], and contains an action
      that transforms the result stack accordingly. *)

open! Core

module Make (G : Grammar_module_types.S) : sig
  module Eps_edge : sig
    type ('src, 'dst) t
    type 'src packed = Packed : ('src, _) t -> 'src packed
  end

  module Token_edge : sig
    include Typed_comparable.S

    type packed = Packed : _ t -> packed [@@deriving sexp_of]
  end

  module Node : sig
    include Typed_comparable.S

    type packed = Packed : _ t -> packed [@@deriving sexp_of]
  end

  type t

  (** Compiles a grammar to Thompson's NFA, returning the start and end nodes.

      The start node expects an empty result stack, i.e. a [unit Heterogeneous_list.t],
      i.e. [[]], while the end node expects a singleton of type ['root], i.e. a
      [('root -> unit) Heterogeneous_list.t], i.e. [[ final_result ]] by the time you
      reach it. *)
  val create : 'root G.t -> t * start:unit Node.t * end_:('root -> unit) Node.t

  (** The list of all token edges *)
  val token_edges : t -> Token_edge.packed list

  (** Epsilon transitions: returns a list of all epsilon edges starting at a node. *)
  val follow_eps : t -> 'a Node.t -> 'a Eps_edge.packed list

  (** Token transitions: returns a list of all token edges starting at a node, and the
      associated terminal symbols. *)
  val follow_token : t -> 'a Node.t -> (G.terminal * 'a Token_edge.t) list

  module Eps_edge_data : sig
    type ('src, 'dst) t =
      { action : ('src, 'dst) Action.Make(G).Epsilon.t
      ; src : 'src Node.t
      ; dst : 'dst Node.t
      }
  end

  module Token_edge_data : sig
    type 'src t =
      { src : 'src Node.t
      ; dst : 'src Node.t
      }
  end

  val eps_edge_data : t -> ('src, 'dst) Eps_edge.t -> ('src, 'dst) Eps_edge_data.t
  val token_edge_data : t -> 'src Token_edge.t -> 'src Token_edge_data.t
end
