open! Core

(** Glushkov's NFA (nondeterministic finite automaton).

    Glushkov's construction produces an epsilon-free NFA where states correspond to
    positions (terminal occurrences) in the grammar. This module builds the Glushkov NFA
    by first constructing Thompson's NFA and then precomputing epsilon closures, which
    yields an equivalent automaton.

    The precomputed tables support both the scanning phase (following token transitions)
    and the reconstruction phase (tracing derivation chains back through Thompson nodes). *)

module Make (G : Grammar_module_types.S) : sig
  (** A [State.t] is a position in the Glushkov automaton, corresponding to a terminal
      occurrence in the grammar (plus an initial state). *)
  module State : Comparable.S_plain

  type t

  (** Compiles a grammar to the NFA, also returning the initial [State.t], and the
      Thompson node to start the reconstruction from. *)
  val create
    :  'root G.t
    -> t * start:State.t * root:('root -> unit) Thompson_nfa.Make(G).Node.t

  (** Advances the [State.t] by an input token. *)
  val follow_token : t -> State.t -> token:G.token -> State.t list

  module Derivation_chain : sig
    module Origin : sig
      type _ t =
        | Start : unit t
        | Node : 'a Thompson_nfa.Make(G).Node.t -> 'a t
      [@@deriving sexp_of]
    end

    type 'dst t =
      | T : 'src Origin.t * ('src, 'dst) Action.Make(G).Epsilon_list.t -> 'dst t
  end

  (** Returns a sequence of valid [Derivation_chain.t] for a given Thompson node at a
      given Glushkov state.

      Concretely, a [T (origin, actions)] in a [derivation_chain t state node] denotes
      that to reconstruct a result stack for [node], one has to start with the result
      stack for [origin] for the previous Glushkov state, and then apply [actions].
      [origin] can be either [Node _] if the state was obtained with [follow_token], or
      [Start] if it is the initial state. *)
  val derivation_chain
    :  t
    -> State.t
    -> 'a Thompson_nfa.Make(G).Node.t
    -> 'a Derivation_chain.t Fast_sequence.t
end
