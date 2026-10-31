open! Core

(** The toplevel interface for the parsing engine.

    Example usage:
    {[
      module Parser = Parser.Make (Grammar)

      let parser = Parser.create grammar in
      let parser = List.fold tokens ~init:parser ~f:Parser.advance in
      let results = Parser.reconstruct parser ~input in
      (* Iterate over results with Fast_sequence.next *)
    ]} *)
module Make (G : Grammar_module_types.S) : sig
  (** A parser for grammars producing values of type ['root]. *)
  type 'root t

  (** [create grammar] compiles [grammar] into a parser ready to accept tokens. *)
  val create : 'root G.t -> 'root t

  (** [advance parser token] feeds [token] to the parser, returning an updated parser. *)
  val advance : 'root t -> G.token -> 'root t

  (** [reconstruct parser] returns a stream of all valid parse results. Call this after
      advancing through all input tokens.

      The parse results are intentionally returned as thunks; the closures inside the
      provided grammar are evaluated only once the thunk is evaluated, so that any
      exceptions there don't prevent the parse result stream from advancing.

      The order in which the results are being returned is stable, and won't be changed
      without an explicit version upgrade. The ordering is somewhat difficult to describe,
      see [test/ordering_tests.ml] for more details. *)
  val reconstruct : 'root t -> input:G.input -> (unit -> 'root) Fast_sequence.t
end
