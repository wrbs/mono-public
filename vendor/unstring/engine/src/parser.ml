open! Core
open! Composition_infix

module Make (G : Grammar_module_types.S) = struct
  module Thompson_nfa = Thompson_nfa.Make (G)
  module Glushkov_nfa = Glushkov_nfa.Make (G)
  module Parse_thread = Parse_thread.Make (G)

  type 'root t =
    { automaton : Glushkov_nfa.t
    ; goal : ('root -> unit) Thompson_nfa.Node.t
    ; threads : Parse_thread.t list
    }

  let create grammar =
    let ~automaton, ~start, ~goal = Parse_thread.create grammar in
    { automaton; goal; threads = [ start ] }
  ;;

  let advance { automaton; goal; threads } token =
    let threads =
      List.concat_map threads ~f:(Parse_thread.advance ~automaton ~token)
      |> Parse_thread.compact
    in
    { automaton; goal; threads }
  ;;

  let reconstruct { automaton; goal; threads } ~input : _ Fast_sequence.t =
    Fast_sequence.of_list threads
    |> Fast_sequence.bind ~f:(fun thread ->
      Parse_thread.reconstruct thread ~automaton ~goal ~input)
  ;;
end
