open! Core

module Make (G : Grammar_module_types.S) = struct
  module Thompson_nfa = Thompson_nfa.Make (G)
  module Glushkov_nfa = Glushkov_nfa.Make (G)
  module Action = Action.Make (G)

  type t =
    { state : Glushkov_nfa.State.t (* The current state in the automaton *)
    ; prevs : t list
        (* The state [repeats+1] tokens ago. (It can be a list due to [compact]) *)
    ; repeats : int
    (* For the optimisation where we don't keep intermediate state if each token keeps it
       unchanged (e.g. [Star_unit (Terminal _)])
    *)
    }

  let create grammar =
    let glushkov_nfa, ~start, ~root = Glushkov_nfa.create grammar in
    let start = { state = start; repeats = 0; prevs = [] } in
    ~start, ~automaton:glushkov_nfa, ~goal:root
  ;;

  (* Raises only if [ts] is empty *)
  let merge_prevs_exn (ts : t list) =
    let state = (List.hd_exn ts).state in
    let min_repeats =
      List.fold ts ~init:Int.max_value ~f:(fun acc t -> Int.min acc t.repeats)
    in
    let prevs =
      List.concat_map ts ~f:(fun { repeats; prevs; state = _ } ->
        if repeats = min_repeats
        then prevs
        else [ { state; repeats = repeats - min_repeats - 1; prevs } ])
    in
    { state; repeats = min_repeats; prevs }
  ;;

  let expand_prevs { state; repeats; prevs } =
    if repeats = 0 then prevs else [ { state; repeats = repeats - 1; prevs } ]
  ;;

  let compact = function
    | [] -> []
    | [ t ] -> [ t ]
    | ts ->
      List.sort_and_group
        ts
        ~compare:(Comparable.lift [%compare: Glushkov_nfa.State.t] ~f:(fun t -> t.state))
      |> List.map ~f:merge_prevs_exn
  ;;

  let advance { state; repeats; prevs } ~automaton:glushkov_nfa ~token =
    let%map.List next = Glushkov_nfa.follow_token glushkov_nfa state ~token in
    if [%compare.equal: Glushkov_nfa.State.t] next state
    then { state; repeats = repeats + 1; prevs }
    else { state = next; repeats = 0; prevs = [ { state; repeats; prevs } ] }
  ;;

  let execute_final_action action ~input () =
    let [ root ], ~pos:_ = Action.Token_separated.execute action ~input ([], ~pos:0) in
    root
  ;;

  let match_sequence_length seq =
    match Fast_sequence.next seq with
    | None -> `Empty
    | Some (a, seq) ->
      (match Fast_sequence.next seq with
       | Some (b, seq) ->
         `Many (Fast_sequence.cons a (lazy (Fast_sequence.cons b (lazy seq))))
       | None -> `One a)
  ;;

  module Reconstruction_state = struct
    type thread = t

    (* The intermediate state during reconstruction. Consists of the current thread, the
       current node, and the actions collected thus far *)
    type ('node, 'root) t =
      { thread : thread
      ; node : 'node Thompson_nfa.Node.t
      ; actions : ('node, 'root -> unit) Action.Token_separated.t
      }

    type 'root packed = T : (_, 'root) t -> 'root packed

    (* How to advance the reconstruction state in the general case *)
    let general_advance
      (type node root)
      ({ thread; node = _; actions } : (node, root) t)
      ~(derivations : node Glushkov_nfa.Derivation_chain.t Fast_sequence.t)
      =
      Fast_sequence.bind derivations ~f:(fun (T (origin, action)) ->
        let actions = Action.Token_separated.cons action actions in
        match origin with
        | Start ->
          `Finish (actions : (unit, root -> unit) Action.Token_separated.t)
          |> Fast_sequence.return
        | Node prev_node ->
          Fast_sequence.of_list_map (expand_prevs thread) ~f:(fun prev_thread ->
            `Continue (T { thread = prev_thread; node = prev_node; actions })))
    ;;

    (* Optimisation: when there is only one action to take, that preserves the state, we
       can handle all the repeats at once instead of iterating one-by-one. In particular,
       it speeds up constructs like [Capture (Star_unit _)].
    *)
    let skip_repeats_advance
      (type node root)
      ({ thread; node = _; actions } : (node, root) t)
      ~(prev_node : node Thompson_nfa.Node.t)
      ~(action : (node, node) Action.Epsilon_list.t)
      ~repeats
      =
      let actions = Action.Token_separated.cons_repeat (action, repeats + 1) actions in
      Fast_sequence.of_list_map thread.prevs ~f:(fun prev_thread ->
        `Continue (T { thread = prev_thread; node = prev_node; actions }))
    ;;

    let advance (type node root) (t : (node, root) t) ~glushkov_nfa =
      match
        Glushkov_nfa.derivation_chain glushkov_nfa t.thread.state t.node
        |> match_sequence_length
      with
      | `Empty -> Fast_sequence.empty
      | `Many derivations -> general_advance t ~derivations
      | `One (T (Start, _) as x) ->
        general_advance t ~derivations:(Fast_sequence.return x)
      | `One (T (Node prev_node, action) as x) ->
        (match t.thread.repeats, Thompson_nfa.Node.compare prev_node t.node with
         | 0, _ | _, (Less | Greater) ->
           general_advance t ~derivations:(Fast_sequence.return x)
         | repeats, Equal T -> skip_repeats_advance t ~prev_node ~action ~repeats)
    ;;
  end

  let reconstruct
    (type root)
    t
    ~automaton:glushkov_nfa
    ~(goal : (root -> unit) Thompson_nfa.Node.t)
    ~input
    : (unit -> root) Fast_sequence.t
    =
    Fast_sequence.recurse
      ~init:
        (Reconstruction_state.T
           { thread = t; node = goal; actions = Action.Token_separated.empty })
      ~f:(fun (T state) -> Reconstruction_state.advance state ~glushkov_nfa)
    |> Fast_sequence.map ~f:(execute_final_action ~input)
  ;;
end
