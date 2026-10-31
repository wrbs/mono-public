open! Core

(** A [Parse_thread.t] includes the current Glushkov state as well as the full data
    necessary to reconstruct a parse. *)
module Make (G : Grammar_module_types.S) : sig
  type t

  (** Compile the provided grammar, returning the initial parse thread, the Glushkov
      automaton, and the goal for reconstructing the final parse. *)
  val create
    :  'a G.t
    -> start:t
       * automaton:Glushkov_nfa.Make(G).t
       * goal:('a -> unit) Thompson_nfa.Make(G).Node.t

  (** Advance the parse thread by a token, nondeterministically. *)
  val advance : t -> automaton:Glushkov_nfa.Make(G).t -> token:G.token -> t list

  (** Compact a collection of parse threads into an equivalent one. The length of the
      resulting list is capped linearly by the size of the grammar. *)
  val compact : t list -> t list

  (** Return the stream of thunks for the reconstructed valid parses. *)
  val reconstruct
    :  t
    -> automaton:Glushkov_nfa.Make(G).t
    -> goal:('a -> unit) Thompson_nfa.Make(G).Node.t
    -> input:G.input
    -> (unit -> 'a) Fast_sequence.t
end
