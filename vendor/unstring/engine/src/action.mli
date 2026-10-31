open! Core

module Make (G : sig
    type input
  end) : sig
  (** The types in this module specify actions to carry out on the result stack as we
      traverse the input stream.

      For any of the types below [('a, 'b) t] signifies an action that transforms an
      ['a Heterogeneous_list.t] into a ['b Heterogeneous_list.t].

      - [Epsilon] denotes an action associated with a single epsilon-transition in the
        [Thompson_nfa]
      - [Epsilon_list] denotes a list of [Epsilon] actions stringed together
      - [Token_separated] strings [Epsilon_list] actions together, with an implicit
        token-transition in-between. *)

  module Epsilon : sig
    type capture_start

    (** - [No_op]: a no-op
        - [Map f]: maps the top items of the stack using [f]
        - [Map2 f]: applies [f] to the top two items on the stack, fusing them together
          into one
        - [Push s]: pushes [s] on top of the stack
        - [Pop_unit]: pops a [()] from the top of the stack. Semantically equivalent to
          [Map2 (fun a () -> a)]
        - [Start_capture] pushes the current position in the input stream onto the stack,
          to be used by [Capture] or [Capture_also]
        - [Capture] replaces the position produced by [Start_capture] with the segment
          that starts at said position and ends at current position
        - [Capture_also] is the same as [Capture], except that it can preserve any
          additional value produced in the meantime *)

    type ('a, 'b) t =
      | No_op : ('stack, 'stack) t
      | Map : ('a -> 'b) -> ('a -> 'stack, 'b -> 'stack) t
      | Map2 : ('a -> 'b -> 'c) -> ('b -> 'a -> 'stack, 'c -> 'stack) t
      | Push : 's -> ('stack, 's -> 'stack) t
      | Pop_unit : (unit -> 'stack, 'stack) t
      | Start_capture : ('stack, capture_start -> 'stack) t
      | Capture :
          ('a, G.input) Grammar_module_types.Capture.t
          -> (capture_start -> 'stack, 'a -> 'stack) t
      | Capture_also :
          ('a -> 'b, G.input) Grammar_module_types.Capture.t
          -> ('a -> capture_start -> 'stack, 'b -> 'stack) t
    [@@deriving sexp_of]
  end

  module Epsilon_list : sig
    type ('a, 'b) t [@@deriving sexp_of]

    val empty : ('a, 'a) t
    val cons : ('a, 'b) Epsilon.t -> ('b, 'c) t -> ('a, 'c) t
  end

  module Token_separated : sig
    type ('a, 'b) t [@@deriving sexp_of]

    val empty : ('a, 'a) t
    val cons : ('a, 'b) Epsilon_list.t -> ('b, 'c) t -> ('a, 'c) t

    (** [cons_repeat (a, n) action] is equivalent to applying [cons a] [n] times, but
        optimised for the case when [a] is a no-op. *)
    val cons_repeat : ('a, 'a) Epsilon_list.t * int -> ('a, 'b) t -> ('a, 'b) t

    (** [execute t ~input (a, ~pos)] runs [t] on [a], advancing the position [pos]
        throughout. ([input] is fed into [Capture] and [Capture_also]) *)

    val execute
      :  ('a, 'b) t
      -> input:G.input
      -> 'a Heterogeneous_list.t * pos:int
      -> 'b Heterogeneous_list.t * pos:int
  end
end
