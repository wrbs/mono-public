open! Core

type 'a t = 'a Defs.Parser.t

module Run_error = struct
  module Caught_exn = struct
    type t = [ `Caught_exn of exn ] [@@deriving sexp_of]
  end

  module Ambiguous = struct
    type 'a t = [ `Ambiguous of 'a * 'a ] [@@deriving sexp_of]
  end

  type 'a t =
    [ Caught_exn.t
    | 'a Ambiguous.t
    ]
  [@@deriving sexp_of]

  let to_error (type a) sexp_of_a : [< a t ] -> Error.t = function
    | `Caught_exn e -> Error.of_exn e
    | `Ambiguous (first, second) ->
      Error.create_s [%message "Ambiguous parse" (first : a) (second : a)]
  ;;
end

module Settings = struct
  module Parse_type = struct
    type ('a, 'b, 'err) t =
      | Id : ('a, 'a, Nothing.t) t
      | Error_handling : ('a, ('a, 'err) Result.t, 'err) t
  end

  module On_exn = struct
    type 'err t =
      | Error : [> Run_error.Caught_exn.t ] t
      | Raise : 'err t

    let run (type a b err) : (a, b, err) Parse_type.t * err t -> (unit -> a) -> b
      = function
      | Id, Raise -> fun f -> f ()
      | Error_handling, Raise -> fun f -> Ok (f ())
      | Error_handling, Error ->
        fun f ->
          (try Ok (f ()) with
           | exn -> Error (`Caught_exn exn))
    ;;
  end

  module On_multiple_parses = struct
    type ('b, 'c, 'err) t =
      | First_parse : ('b, 'b option, 'err) t
      | Error_if_ambiguous : ('b, 'b option, [> 'b Run_error.Ambiguous.t ]) t
      | Raise_if_ambiguous : ('b -> Sexp.t) -> ('b, 'b option, 'err) t
      | All_parses : ('b, 'b list, 'err) t
      | Parse_sequence : ('b, 'b Sequence.t, 'err) t

    let run (type a b c err)
      : (a, b, err) Parse_type.t * (b, c, err) t -> b Unstring_engine.Fast_sequence.t -> c
      =
      let next s = Unstring_engine.Fast_sequence.next s in
      function
      | _, First_parse -> fun s -> next s |> Option.map ~f:fst
      | Error_handling, Error_if_ambiguous ->
        fun s ->
          let%map.Option a, s = next s in
          (match next s with
           | None -> a
           | Some (b, _) -> Error (`Ambiguous (a, b)))
      | _, Raise_if_ambiguous sexp_of_b ->
        fun s ->
          let%map.Option first, s = next s in
          (match next s with
           | None -> first
           | Some (second, _) ->
             raise_s [%message "Ambiguous parse" (first : b) (second : b)])
      | _, All_parses -> fun s -> Sequence.unfold ~init:s ~f:next |> Sequence.to_list
      | _, Parse_sequence -> fun s -> Sequence.unfold ~init:s ~f:next
    ;;
  end

  type ('a, 'b, 'c, 'err) t =
    { parse_type : ('a, 'b, 'err) Parse_type.t
    ; on_exn : 'err On_exn.t
    ; on_multiple_parses : ('b, 'c, 'err) On_multiple_parses.t
    }
end

let run
  (type a b c err)
  (t : a t)
  str
  ~settings:({ parse_type; on_exn; on_multiple_parses } : (a, b, c, err) Settings.t)
  : c
  =
  String.fold str ~init:t ~f:Defs.Parser.advance
  |> Defs.Parser.reconstruct ~input:str
  |> Unstring_engine.Fast_sequence.map ~f:(Settings.On_exn.run (parse_type, on_exn))
  |> Settings.On_multiple_parses.run (parse_type, on_multiple_parses)
;;

module Private = struct
  let create t = t
end
