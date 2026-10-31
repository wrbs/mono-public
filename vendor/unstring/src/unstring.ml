open! Core
module Char_filter = Char_filter
module Bound = Bound

module How_to_capture = struct
  type ('a, 'b) t =
    | Unit : (unit, string) t
    | Full : ('a -> input:string -> pos:int -> len:int -> 'b) -> ('a, 'b) t
end

module rec T : sig
  type 'a t =
    | Return : 'a -> 'a t
    | Concat_unit : unit t list -> unit t
    | Map_list : 'args Hlist.t * ('args Heterogeneous_list.t -> 'a) -> 'a t
    | Char : Char_filter.t -> unit t
    | Capture : 'a t * ('a, 'b) How_to_capture.t -> 'b t
    | Literal : string -> unit t
    | Choice : 'a t list -> 'a t
    | Fold : ('acc, _) Fold_spec.t -> 'acc t
end =
  T

and Hlist : (Heterogeneous_list.S1 with type 'a Element.t = 'a T.t) =
  Heterogeneous_list.Make (T)

and Fold_spec : sig
  type ('acc, 'item) t =
    { how_to_fold : ('acc, 'item) Fold.How_to_fold.t
    ; bounds : Bound.t * Bound.t
    ; item : 'item T.t
    ; sep : unit T.t option
    }
end =
  Fold_spec

include T

let return v = Return v
let map2 a b ~f = Map_list ([ a; b ], fun [ a; b ] -> f a b)
let map = `Custom (fun a ~f -> Map_list ([ a ], fun [ a ] -> f a))

include functor Applicative.Make_using_map2

let all_unit ts = Concat_unit ts
let unbounded : Bound.t * Bound.t = Unbounded, Unbounded

module Open_on_rhs = struct
  let char c = Char c
  let lit str = Literal str

  let star t =
    Fold { how_to_fold = Unit; bounds = Incl 0, Unbounded; item = t; sep = None }
  ;;

  let multiple ?sep ?(bounds = unbounded) t =
    Fold { how_to_fold = Unit; bounds; item = t; sep }
  ;;

  let capture t = Capture (t, Unit)
  let capture' t ~f = Capture (t, Full f)

  let list ?sep ?(bounds = unbounded) t =
    Map_list
      ( [ Fold
            { how_to_fold = Fold { init = []; f = (fun xs x -> x :: xs) }
            ; bounds
            ; item = t
            ; sep
            }
        ]
      , fun [ list ] -> List.rev list )
  ;;

  let fold ?sep ?(bounds = unbounded) t ~init ~f =
    Fold { how_to_fold = Fold { init; f }; bounds; item = t; sep }
  ;;

  let choice ts = Choice ts
  let map_hlist args ~f = Map_list (args, f)

  let option some ~none =
    choice [ map some ~f:(fun x -> Some x); map none ~f:(fun () -> None) ]
  ;;

  let bool =
    choice
      [ map (lit "false") ~f:(fun () -> false); map (lit "true") ~f:(fun () -> true) ]
  ;;

  let sign_prefix = choice [ lit "-"; lit "+"; return () ]

  let int =
    [ sign_prefix; multiple ~bounds:(Incl 1, Unbounded) (char Digit) ]
    |> all_unit
    |> capture
    |> map ~f:Int.of_string
  ;;

  let float =
    let digit_or_dot = function
      | '0' .. '9' | '.' -> true
      | _ -> false
    in
    let is_e = function
      | 'e' | 'E' -> true
      | _ -> false
    in
    let body = multiple ~bounds:(Incl 1, Unbounded) (char (Fun digit_or_dot)) in
    let exp = multiple ~bounds:(Incl 1, Unbounded) (char Digit) in
    let main =
      all_unit
        [ body; choice [ return (); all_unit [ char (Fun is_e); sign_prefix; exp ] ] ]
    in
    all_unit [ sign_prefix; choice [ main; lit "nan"; lit "inf" ] ]
    |> capture
    |> map ~f:Float.of_string
  ;;

  let string ?(chars = Char_filter.Any) () = multiple (char chars) |> capture
end

module Open_on_rhs_intf = struct
  module type S = sig
    val char : Char_filter.t -> unit t
    val lit : string -> unit t
    val star : unit t -> unit t
    val multiple : ?sep:unit t -> ?bounds:Bound.t * Bound.t -> unit t -> unit t
    val capture : unit t -> string t
    val capture' : 'a t -> f:('a -> input:string -> pos:int -> len:int -> 'b) -> 'b t
    val list : ?sep:unit t -> ?bounds:Bound.t * Bound.t -> 'a t -> 'a list t

    val fold
      :  ?sep:unit t
      -> ?bounds:Bound.t * Bound.t
      -> 'a t
      -> init:'acc
      -> f:('acc -> 'a -> 'acc)
      -> 'acc t

    val choice : 'a t list -> 'a t
    val map_hlist : 'shape Hlist.t -> f:('shape Heterogeneous_list.t -> 'a) -> 'a t
    val option : 'a t -> none:unit t -> 'a option t
    val bool : bool t
    val int : int t
    val float : float t
    val string : ?chars:Char_filter.t -> unit -> string t
  end
end

include Open_on_rhs

[%%self: module Self : Applicative.For_let_syntax with type 'a t = 'a t]

include functor Applicative.Make_let_syntax (Self) (Open_on_rhs_intf)

let rec build_grammar : type a. a t -> a Defs.Grammar.t = function
  | Return x -> Return x
  | Concat_unit ts -> Concat_unit (List.map ts ~f:build_grammar)
  | Map_list (args, f) ->
    let rec parsers : type args. args Hlist.t -> args Heterogeneous_list.t Defs.Grammar.t
      = function
      | [] -> Return Heterogeneous_list.[]
      | [ a ] -> Map (build_grammar a, fun a -> Heterogeneous_list.[ a ])
      | a :: rest -> Append (build_grammar a, parsers rest, fun a tail -> a :: tail)
    in
    Map (parsers args, f)
  | Char c -> Terminal c
  | Capture (t, Unit) -> Capture (build_grammar t, String.sub)
  | Capture (t, Full f) ->
    Capture_also (build_grammar t, fun input ~pos ~len a -> f a ~input ~pos ~len)
  | Literal str ->
    Concat_unit
      (List.map (String.to_list str) ~f:(fun c -> Defs.Grammar.Terminal (Char c)))
  | Choice ts -> Union (List.map ts ~f:build_grammar)
  | Fold { how_to_fold; bounds; item; sep } ->
    Fold.build_grammar
      { how_to_fold
      ; bounds
      ; item = build_grammar item
      ; sep = Option.map sep ~f:build_grammar
      }
;;

module Compiled = struct
  type 'a t = 'a Compiled_expert.t

  let run_unambiguous (type a) sexp_of_a (t : a t) str : a Or_error.t option =
    match
      Compiled_expert.run
        t
        str
        ~settings:
          { parse_type = Error_handling
          ; on_exn = Error
          ; on_multiple_parses = Error_if_ambiguous
          }
    with
    | None -> None
    | Some (Ok x) -> Some (Ok x)
    | Some (Error err) ->
      let err =
        Compiled_expert.Run_error.to_error
          [%sexp_of: (a, _ Compiled_expert.Run_error.t) Result.t]
          err
      in
      Some (Error err)
  ;;

  let run_all_parses (type a) (t : a t) str : a Or_error.t Sequence.t =
    Compiled_expert.run
      t
      str
      ~settings:
        { parse_type = Error_handling
        ; on_exn = Error
        ; on_multiple_parses = Parse_sequence
        }
    |> Sequence.map ~f:(function
      | Ok x -> Ok x
      | Error (`Caught_exn exn) -> Error (Error.of_exn exn))
  ;;

  module Expert = Compiled_expert
end

let compile (type a) (t : a t) : a Compiled.t =
  Defs.Parser.create (build_grammar t) |> Compiled_expert.Private.create
;;

let make_of_string (type a) (t : a t) ~compile_when ~on_ambiguity =
  let compiled =
    match compile_when with
    | `Now -> compile t |> Lazy.from_val
    | `Lazily -> lazy (compile t)
  in
  Staged.stage (fun str ->
    match
      Compiled_expert.run
        (force compiled)
        str
        ~settings:
          { parse_type = Id
          ; on_exn = Raise
          ; on_multiple_parses =
              (match on_ambiguity with
               | `Raise sexp_of -> Raise_if_ambiguous sexp_of
               | `Pick_first -> First_parse)
          }
    with
    | Some a -> a
    | None -> raise_s [%message "Unstring: no parse found"])
;;

module Export = struct
  (* Named parsers for ppx_unstring *)
  let unstring_int = int
  let unstring_float = float
  let unstring_bool = bool
  let unstring_string = string ()
  let unstring_string_chars ~chars = string ~chars ()
  let unstring_unit ~unit = unit
  let unstring_list ~sep inner = list ~sep inner
  let unstring_option ~some ~none inner = option (some inner) ~none

  (* Named restring functions for ppx_unstring ~restring *)
  let restring_int = Int.to_string
  let restring_float = Float.to_string
  let restring_bool = Bool.to_string
  let restring_string = Fn.id
  let restring_unit ~unit:s () = s
  let restring_list ~sep f xs = List.map xs ~f |> String.concat ~sep

  let restring_option ~some ~none f = function
    | Some x -> some f x
    | None -> none
  ;;
end

module Printable_shape = Printable_shape

let to_printable_shape t = build_grammar t |> Printable_shape.of_grammar
let print_shape t = t |> to_printable_shape |> Printable_shape.to_string |> print_endline
