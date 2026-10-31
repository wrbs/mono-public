open! Core

module Make (G : Grammar_module_types.S) = struct
  module Token_lookup = G.Token_lookup
  module Thompson_nfa = Thompson_nfa.Make (G)
  module State = Int
  module Action = Action.Make (G)

  module Table = struct
    type t =
      | Empty
      | Terminal of G.terminal * State.t
      | Table of State.t list Token_lookup.t
    [@@deriving sexp_of]

    let create : (G.terminal * State.t) list -> t = function
      | [] -> Empty
      | [ (terminal, state) ] -> Terminal (terminal, state)
      | by_terminal ->
        Token_lookup.create (fun token ->
          List.filter_map by_terminal ~f:(fun (terminal, next) ->
            if G.matches terminal token then Some next else None))
        |> Table
    ;;

    let find t token =
      match t with
      | Empty -> []
      | Terminal (terminal, state) -> if G.matches terminal token then [ state ] else []
      | Table table -> Token_lookup.find table token
    ;;
  end

  module Origin = struct
    type _ t =
      | Start : unit t
      | Node : 'a Thompson_nfa.Node.t -> 'a t
    [@@deriving sexp_of]
  end

  module Derivation = struct
    type 'dst t =
      | Origin : 'a Origin.t -> 'a t
      | Step : 'src Thompson_nfa.Node.t * ('src, 'dst) Action.Epsilon.t -> 'dst t
    [@@deriving sexp_of]
  end

  module Derivation_map =
    Typed_comparable.Make_map
      (Thompson_nfa.Node)
      (struct
        type 'a t = 'a Derivation.t list [@@deriving sexp_of]
      end)

  module Derivation_chain = struct
    module Origin = Origin

    type 'dst t =
      | T : 'src Origin.t * (('src, 'dst) Action.Epsilon_list.t[@opaque]) -> 'dst t
    [@@deriving sexp_of]
  end

  module Derivation_chain_map =
    Typed_comparable.Make_map
      (Thompson_nfa.Node)
      (struct
        type 'a t = 'a Derivation_chain.t Fast_sequence.t [@@deriving sexp_of]
      end)

  let create_derivation_seq
    (type dst)
    derivation_map
    (thompson_node : dst Thompson_nfa.Node.t)
    : dst Derivation_chain.t Fast_sequence.t
    =
    let module Acc = struct
      type 'dst t =
        | T : 'src Thompson_nfa.Node.t * ('src, 'dst) Action.Epsilon_list.t -> 'dst t
    end
    in
    Fast_sequence.recurse
      ~init:(Acc.T (thompson_node, Action.Epsilon_list.empty))
      ~f:(fun (T (thompson_node, action)) ->
        Derivation_map.find derivation_map thompson_node
        |> Option.value ~default:[]
        |> Fast_sequence.of_list_map ~f:(function
          | Origin src -> `Finish (Derivation_chain.T (src, action))
          | Step (src, a) ->
            let action = Action.Epsilon_list.cons a action in
            `Continue (Acc.T (src, action))))
    |> Fast_sequence.cache
  ;;

  module Data = struct
    type t =
      { follow_table : Table.t
      ; derivation_chains : Derivation_chain_map.t
      }
    [@@deriving sexp_of]
  end

  type t = Data.t Iarray.t [@@deriving sexp_of]

  let derivations ~nfa thompson_node origin =
    let rec loop
      : type a. acc:Derivation_map.t -> a Thompson_nfa.Node.t -> Derivation_map.t
      =
      fun ~acc thompson_node ->
      Thompson_nfa.follow_eps nfa thompson_node
      |> List.fold ~init:acc ~f:(fun acc (Packed eps_edge) ->
        let { Thompson_nfa.Eps_edge_data.src; dst; action } =
          Thompson_nfa.eps_edge_data nfa eps_edge
        in
        let derivation = Derivation.Step (src, action) in
        match Derivation_map.find acc dst with
        | None ->
          let acc = Derivation_map.set acc ~key:dst ~data:[ derivation ] in
          loop ~acc dst
        | Some entries -> Derivation_map.set acc ~key:dst ~data:(derivation :: entries))
    in
    let acc =
      Derivation_map.set Derivation_map.empty ~key:thompson_node ~data:[ origin ]
    in
    loop ~acc thompson_node |> Derivation_map.map ~f:List.rev
  ;;

  module Token_edge_index = struct
    type t = Thompson_nfa.Token_edge.packed [@@deriving sexp_of]

    let compare (Packed a : t) (Packed b : t) =
      Thompson_nfa.Token_edge.compare a b
      |> Typed_comparable.Ordering.untyped
      |> Ordering.to_int
    ;;

    include functor Comparable.Make_plain
  end

  let create grammar =
    let nfa, ~start:start_node, ~end_:root_node = Thompson_nfa.create grammar in
    let token_edges = Thompson_nfa.token_edges nfa in
    let token_edge_mapping =
      List.mapi token_edges ~f:(fun i c -> c, i + 1) |> Token_edge_index.Map.of_alist_exn
    in
    let start = 0 in
    let data =
      `Start :: List.map token_edges ~f:(fun e -> `Token_edge e)
      |> List.map ~f:(fun state_origin ->
        let derivation_map =
          match state_origin with
          | `Start -> derivations ~nfa start_node (Origin Start)
          | `Token_edge (Packed e) ->
            let { Thompson_nfa.Token_edge_data.src; dst } =
              Thompson_nfa.token_edge_data nfa e
            in
            derivations ~nfa dst (Origin (Node src))
        in
        let thompson_nodes =
          Derivation_map.keys derivation_map (fun p -> Thompson_nfa.Node.Packed p)
        in
        let derivation_chains =
          thompson_nodes
          |> List.fold
               ~init:Derivation_chain_map.empty
               ~f:(fun acc (Packed thompson_node) ->
                 let (stream : _ Fast_sequence.t) =
                   create_derivation_seq derivation_map thompson_node
                 in
                 Derivation_chain_map.set acc ~key:thompson_node ~data:stream)
        in
        let follow_table =
          List.concat_map thompson_nodes ~f:(fun (Packed thompson_node) ->
            Thompson_nfa.follow_token nfa thompson_node
            |> List.map ~f:(fun (terminal, token_edge) ->
              terminal, Map.find_exn token_edge_mapping (Packed token_edge)))
          |> Table.create
        in
        { Data.follow_table; derivation_chains })
      |> Iarray.of_list
    in
    data, ~start, ~root:root_node
  ;;

  let follow_token (t : t) state ~token = Table.find t.:(state).follow_table token

  let derivation_chain (type a) (t : t) state (thompson_node : a Thompson_nfa.Node.t)
    : a Derivation_chain.t Fast_sequence.t
    =
    Derivation_chain_map.find t.:(state).derivation_chains thompson_node
    |> Option.value ~default:Fast_sequence.empty
  ;;
end
