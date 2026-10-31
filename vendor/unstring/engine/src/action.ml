open! Core

module Make (G : sig
    type input
  end) =
struct
  module Epsilon = struct
    (* The input position at the start of the capture group in question *)
    type capture_start = int

    type ('a, 'b) t =
      | No_op : ('stack, 'stack) t
      | Map : ('a -> 'b) -> ('a -> 'stack, 'b -> 'stack) t
      | Map2 : ('a -> 'b -> 'c) -> ('b -> 'a -> 'stack, 'c -> 'stack) t
      | Push : ('s[@opaque]) -> ('stack, 's -> 'stack) t
      | Pop_unit : (unit -> 'stack, 'stack) t
      | Start_capture : ('stack, capture_start -> 'stack) t
      | Capture :
          (('a, G.input) Grammar_module_types.Capture.t[@opaque])
          -> (capture_start -> 'stack, 'a -> 'stack) t
      | Capture_also :
          (('a -> 'b, G.input) Grammar_module_types.Capture.t[@opaque])
          -> ('a -> capture_start -> 'stack, 'b -> 'stack) t
    [@@deriving sexp_of]

    let execute
      : type a b.
        (a, b) t
        -> #(pos:int * input:G.input)
        -> a Heterogeneous_list.t
        -> b Heterogeneous_list.t
      = function
      | No_op -> fun _ stack -> stack
      | Map f -> fun _ (a :: stack) -> f a :: stack
      | Map2 f -> fun _ (b :: a :: stack) -> f a b :: stack
      | Push s -> fun _ stack -> s :: stack
      | Pop_unit -> fun _ (() :: stack) -> stack
      | Start_capture -> fun #(~pos:start, ~input:_) stack -> start :: stack
      | Capture capture ->
        fun #(~pos:end_, ~input) (start :: stack) ->
          capture input ~pos:start ~len:(end_ - start) :: stack
      | Capture_also capture ->
        fun #(~pos:end_, ~input) (a :: start :: stack) ->
          capture input ~pos:start ~len:(end_ - start) a :: stack
    ;;
  end

  module Epsilon_list = struct
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : ('a, 'b) Epsilon.t * ('b, 'c) t -> ('a, 'c) t
    [@@deriving sexp_of]

    let empty = []

    let cons : type a b c. (a, b) Epsilon.t -> (b, c) t -> (a, c) t =
      fun l r ->
      match l, r with
      | No_op, r -> r
      | l, r -> l :: r
    ;;

    let rec execute
      : type a b.
        (a, b) t
        -> #(pos:int * input:G.input)
        -> a Heterogeneous_list.t
        -> b Heterogeneous_list.t
      =
      fun t #(~pos, ~input) a ->
      match t with
      | [] -> a
      | t :: ts -> a |> Epsilon.execute t #(~pos, ~input) |> execute ts #(~pos, ~input)
    ;;
  end

  module Token_separated = struct
    type ('a, 'b) t =
      | Empty : ('a, 'a) t
      | Cons : ('a, 'b) Epsilon_list.t * ('b, 'c) t -> ('a, 'c) t
      | Cons_repeat : ('a, 'a) Epsilon_list.t * int * ('a, 'b) t -> ('a, 'b) t
    [@@deriving sexp_of]

    let empty = Empty

    let cons : type a b c. (a, b) Epsilon_list.t -> (b, c) t -> (a, c) t =
      fun l r -> Cons (l, r)
    ;;

    let cons_repeat : type a b. (a, a) Epsilon_list.t * int -> (a, b) t -> (a, b) t =
      fun (l, i) r -> Cons_repeat (l, i, r)
    ;;

    let rec execute
      : type a b.
        (a, b) t
        -> input:G.input
        -> a Heterogeneous_list.t * pos:int
        -> b Heterogeneous_list.t * pos:int
      =
      fun t ~input (a, ~pos) ->
      match t with
      | Empty -> a, ~pos
      | Cons (l, r) ->
        let mid = Epsilon_list.execute l #(~pos, ~input) a in
        execute r ~input (mid, ~pos:(pos + 1))
      | Cons_repeat ([], repeats, r) -> execute r ~input (a, ~pos:(pos + repeats))
      | Cons_repeat (l, repeats, r) ->
        let target = pos + repeats in
        let rec loop (a, ~pos) =
          if pos >= target
          then a, ~pos
          else (Epsilon_list.execute l #(~pos, ~input) a, ~pos:(pos + 1)) |> loop
        in
        loop (a, ~pos) |> execute r ~input
    ;;
  end
end
