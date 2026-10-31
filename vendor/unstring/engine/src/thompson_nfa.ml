open! Core

module Make (G : Grammar_module_types.S) = struct
  module Action = Action.Make (G)

  module Types (Key : T1) = struct
    type !'a node = |
    type (!'src, !'dst) eps_edge = |
    type !'src token_edge = Token_edge

    let _ = Token_edge

    module Eps_edge_data = struct
      type ('src, 'dst) t =
        { action : ('src, 'dst) Action.Epsilon.t
        ; src : 'src node Key.t
        ; dst : 'dst node Key.t
        }
    end

    module Token_edge_data = struct
      type 'src t =
        { src : 'src node Key.t
        ; dst : 'src node Key.t
        }
    end

    module Outward_eps_edge = struct
      type 'src t = T : ('src, 'dst) eps_edge Key.t -> 'src t
    end

    module Node_data = struct
      type 'a t =
        { epsilon : 'a Outward_eps_edge.t Reversed_list.t
        ; by_terminal : (G.terminal * 'a token_edge Key.t) Reversed_list.t
        }

      let empty () = { epsilon = []; by_terminal = [] }
      let add_eps_edge t ~edge = { t with epsilon = edge :: t.epsilon }

      let add_token_edge t ~edge ~terminal =
        { t with by_terminal = (terminal, edge) :: t.by_terminal }
      ;;
    end

    module Data = struct
      type 'a t =
        | Eps_edge : ('src, 'dst) Eps_edge_data.t -> ('src, 'dst) eps_edge t
        | Token_edge : 'src Token_edge_data.t -> 'src token_edge t
        | Node : 'a Node_data.t -> 'a node t

      let add_eps_edge (Node data : _ node t) ~edge : _ node t =
        Node (Node_data.add_eps_edge data ~edge)
      ;;

      let add_token_edge (Node data : _ node t) ~edge ~terminal : _ node t =
        Node (Node_data.add_token_edge data ~edge ~terminal)
      ;;
    end
  end

  module State = Typed_indexer.Make (struct
      module Make (Key : T1) = struct
        module Types = Types (Key)
        include Types.Data
      end
    end)

  include Types (State.Key)
  module Key = State.Key

  let fresh_node () = State.Builder.add (Node (Node_data.empty ()))

  let new_eps_edge
    (type src dst)
    ~(src : src node Key.t)
    ~(dst : dst node Key.t)
    ~(action : (src, dst) Action.Epsilon.t)
    : (src, dst) eps_edge Key.t State.Builder.t
    =
    let%bind.State.Builder edge = State.Builder.add (Eps_edge { action; src; dst }) in
    let%map.State.Builder () =
      State.Builder.update src ~f:(Data.add_eps_edge ~edge:(T edge))
    in
    edge
  ;;

  let new_token_edge (type src) ~(src : src node Key.t) ~(dst : src node Key.t) ~terminal
    : src token_edge Key.t State.Builder.t
    =
    let%bind.State.Builder edge = State.Builder.add (Token_edge { src; dst }) in
    let%map.State.Builder () =
      State.Builder.update src ~f:(Data.add_token_edge ~edge ~terminal)
    in
    edge
  ;;

  let new_node ~src ~action =
    let%bind.State.Builder dst = fresh_node () in
    let%map.State.Builder () = new_eps_edge ~src ~dst ~action |> State.Builder.ignore_m in
    dst
  ;;

  let rec of_grammar
    : type a stack. stack node Key.t -> a G.t -> (a -> stack) node Key.t State.Builder.t
    =
    fun entrance ->
    let default_unit t =
      let%bind.State.Builder exit = of_grammar_unit entrance t in
      new_node ~src:exit ~action:(Push ())
    in
    function
    | Return x -> new_node ~src:entrance ~action:(Push x)
    | Map (t, f) ->
      let%bind.State.Builder exit = of_grammar entrance t in
      new_node ~src:exit ~action:(Map f)
    | Capture (t, capture) ->
      let%bind.State.Builder entrance = new_node ~src:entrance ~action:Start_capture in
      let%bind.State.Builder exit = of_grammar_unit entrance t in
      new_node ~src:exit ~action:(Capture capture)
    | Capture_also (t, capture) ->
      let%bind.State.Builder entrance = new_node ~src:entrance ~action:Start_capture in
      let%bind.State.Builder exit = of_grammar entrance t in
      new_node ~src:exit ~action:(Capture_also capture)
    | Union ts ->
      let%bind.State.Builder exits =
        List.map ts ~f:(of_grammar entrance) |> State.Builder.all
      in
      let%bind.State.Builder final_exit = fresh_node () in
      let%map.State.Builder () =
        List.map exits ~f:(fun src ->
          new_eps_edge ~src ~dst:final_exit ~action:No_op |> State.Builder.ignore_m)
        |> State.Builder.all_unit
      in
      final_exit
    | Append (a, b, f) ->
      let%bind.State.Builder mid = of_grammar entrance a in
      let%bind.State.Builder exit = of_grammar mid b in
      new_node ~src:exit ~action:(Map2 f)
    | Star (t, { init; f }) ->
      let%bind.State.Builder star_entrance = new_node ~src:entrance ~action:(Push init) in
      let%bind.State.Builder star_exit = of_grammar star_entrance t in
      let%map.State.Builder () =
        new_eps_edge ~src:star_exit ~dst:star_entrance ~action:(Map2 f)
        |> State.Builder.ignore_m
      in
      star_entrance
    | Concat_unit _ as t -> default_unit t
    | Terminal _ as t -> default_unit t
    | Star_unit _ as t -> default_unit t

  and of_grammar_unit
    : type stack. stack node Key.t -> unit G.t -> stack node Key.t State.Builder.t
    =
    fun entrance ->
    let default t =
      let%bind.State.Builder exit = of_grammar entrance t in
      new_node ~src:exit ~action:Pop_unit
    in
    function
    | Return () -> State.Builder.return entrance
    | Terminal terminal ->
      let%bind.State.Builder exit = fresh_node () in
      let%map.State.Builder () =
        new_token_edge ~src:entrance ~dst:exit ~terminal |> State.Builder.ignore_m
      in
      exit
    | Union ts ->
      let%bind.State.Builder exits =
        List.map ts ~f:(of_grammar_unit entrance) |> State.Builder.all
      in
      let%bind.State.Builder final_exit = fresh_node () in
      let%map.State.Builder () =
        List.map exits ~f:(fun src ->
          new_eps_edge ~src ~dst:final_exit ~action:No_op |> State.Builder.ignore_m)
        |> State.Builder.all_unit
      in
      final_exit
    | Concat_unit ts ->
      List.fold ts ~init:(State.Builder.return entrance) ~f:(fun acc t ->
        let%bind.State.Builder entrance = acc in
        of_grammar_unit entrance t)
    | Star_unit t ->
      let%bind.State.Builder star_entrance = new_node ~src:entrance ~action:No_op in
      let%bind.State.Builder star_exit = of_grammar_unit star_entrance t in
      let%map.State.Builder () =
        new_eps_edge ~src:star_exit ~dst:star_entrance ~action:No_op
        |> State.Builder.ignore_m
      in
      star_entrance
    | Map _ as t -> default t
    | Capture _ as t -> default t
    | Capture_also _ as t -> default t
    | Append _ as t -> default t
    | Star _ as t -> default t
  ;;

  module Eps_edge = struct
    type ('src, 'dst) t = ('src, 'dst) eps_edge Key.t
    type 'src packed = Packed : ('src, _) t -> 'src packed
  end

  module Token_edge = struct
    type 'src t = ('src token_edge[@opaque]) Key.t [@@deriving sexp_of]

    let compare (type a b) (a : a t) (b : b t) : (a, b) Typed_comparable.Ordering.t =
      match Key.compare a b with
      | Less -> Less
      | Greater -> Greater
      | Equal T -> Equal T
    ;;

    type packed = Packed : _ t -> packed

    let sexp_of_packed (Packed t) = [%sexp_of: _ t] t
  end

  module Node = struct
    type 'a t = ('a node[@opaque]) Key.t [@@deriving sexp_of]

    let compare (type a b) (a : a t) (b : b t) : (a, b) Typed_comparable.Ordering.t =
      match Key.compare a b with
      | Less -> Less
      | Greater -> Greater
      | Equal T -> Equal T
    ;;

    type packed = Packed : _ t -> packed

    let sexp_of_packed (Packed t) = [%sexp_of: _ t] t
  end

  type t = State.t

  let create grammar =
    let (start_node, end_node), state =
      State.build
        (let%bind.State.Builder start_node = fresh_node () in
         let%map.State.Builder end_node = of_grammar start_node grammar in
         start_node, end_node)
    in
    state, ~start:start_node, ~end_:end_node
  ;;

  let token_edges t =
    let extract (type a) : a Key.t -> a Data.t -> Token_edge.packed option =
      fun key data ->
      match data with
      | Node _ -> None
      | Eps_edge _ -> None
      | Token_edge _ -> Some (Packed key)
    in
    State.to_alist t extract |> List.filter_opt
  ;;

  let follow_eps (type a) (t : t) (node : a Node.t) : a Eps_edge.packed list =
    let (Node { epsilon; _ }) = State.find t node in
    Reversed_list.rev_map epsilon ~f:(fun (T edge) -> Eps_edge.Packed edge)
  ;;

  let follow_token (type a) (t : t) (node : a Node.t) : (G.terminal * a Token_edge.t) list
    =
    let (Node { by_terminal; _ }) = State.find t node in
    Reversed_list.rev by_terminal
  ;;

  let eps_edge_data (type src dst) (t : t) (eps_edge : (src, dst) Eps_edge.t)
    : (src, dst) Eps_edge_data.t
    =
    let (Eps_edge data) = State.find t eps_edge in
    data
  ;;

  let token_edge_data (type src) (t : t) (token_edge : src Token_edge.t)
    : src Token_edge_data.t
    =
    let (Token_edge data) = State.find t token_edge in
    data
  ;;
end
