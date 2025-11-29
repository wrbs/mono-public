open! Core

(* A helper type for getting structure out of errors *)

let sexp_to_json sexp = `String (Sexp.to_string_mach sexp)

module Payload = struct
  type t =
    | Single of Sexp.t
    | Tags of (string * Sexp.t) list
    | Other of Sexp.t list
  [@@deriving sexp_of]

  let parse (sexps : Sexp.t list) =
    match sexps with
    | [] -> None
    | _ ->
      Some
        (match
           List.map sexps ~f:(function
             | List [ Atom key; value ] -> Some (key, value)
             | _ -> None)
           |> Option.all
         with
         | Some tags -> Tags tags
         | None ->
           (match sexps with
            | [ sexp ] -> Single sexp
            | _ -> Other sexps))
  ;;

  let to_json t =
    match t with
    | Single sexp -> sexp_to_json sexp
    | Tags tags -> `Object (List.map tags ~f:(Tuple2.map_snd ~f:sexp_to_json))
    | Other sexps -> `Array (List.map sexps ~f:sexp_to_json)
  ;;
end

module Message = struct
  type t =
    | Message of
        { message : string
        ; payload : Payload.t option
        }
    | Other of Sexp.t list
  [@@deriving sexp_of]

  let of_sexp (sexp : Sexp.t) =
    match sexp with
    | Atom message -> Message { message; payload = None }
    | List (Atom message :: rest) -> Message { message; payload = Payload.parse rest }
    | List l -> Other l
  ;;

  let of_string message = Message { message; payload = None }

  let of_tag_arg message sexp ~code_pos =
    Message
      { message
      ; payload =
          Some
            (match code_pos with
             | None -> Single sexp
             | Some pos -> Other [ sexp; Atom (Source_code_position.to_string pos) ])
      }
  ;;

  let to_json = function
    | Message { message; payload = None } -> `String message
    | Message { message; payload = Some payload } ->
      `Array [ `String message; Payload.to_json payload ]
    | Other sexps -> `Array (List.map sexps ~f:sexp_to_json)
  ;;
end

type t =
  | Message of Message.t
  | Wrapped of Message.t * t
  | Multi of t list
  | With_backtrace of t * string list
[@@deriving sexp_of]

let of_sexp sexp = Message (Message.of_sexp sexp)
let of_string message = Message (Message.of_string message)

let rec of_info_repr : Info.Internal_repr.t -> t = function
  | Could_not_construct sexp -> of_sexp sexp
  | String s -> of_string s
  | Exn exn -> of_sexp [%sexp (exn.global : exn)]
  | Sexp sexp -> of_sexp sexp
  | Tag_sexp (message, sexp, code_pos) ->
    Message (Message.of_tag_arg message sexp ~code_pos)
  | Tag_t (message, t) -> Wrapped (Message { message; payload = None }, of_info_repr t)
  | Tag_arg ("", sexp, t) -> Wrapped (Message.of_sexp sexp, of_info_repr t)
  | Tag_arg (message, sexp, t) ->
    Wrapped (Message.of_tag_arg message sexp ~code_pos:None, of_info_repr t)
  | Of_list (_, l) -> Multi (List.map l ~f:of_info_repr)
  | With_backtrace (t, backtrace) ->
    With_backtrace (of_info_repr t, String.split_lines backtrace)
;;

let of_info i = of_info_repr (Info.Internal_repr.of_info i)
let of_error e = of_info (Error.to_info e)

let append_maybe_join json append =
  match json with
  | `Array xs -> `Array (xs @ [ append ])
  | x -> `Array [ x; append ]
;;

let backtrace_json backtrace = `Array (List.map backtrace ~f:(fun l -> `String l))

let rec to_json = function
  | Message message -> Message.to_json message
  | Wrapped (message, t) -> append_maybe_join (Message.to_json message) (to_json t)
  | Multi ts -> `Array (List.map ts ~f:to_json)
  | With_backtrace (t, backtrace) ->
    append_maybe_join (to_json t) (backtrace_json backtrace)
;;

let append_to_data data append =
  match data with
  | None -> append
  | Some existing -> append_maybe_join existing append
;;

let rec to_message_and_data = function
  | Message (Message { message; payload }) ->
    message, Option.map payload ~f:Payload.to_json
  | Message (Other sexps) -> Sexp.to_string_mach (List sexps), None
  | Wrapped (m, t) ->
    let message, data = to_message_and_data (Message m) in
    message, Some (append_to_data data (to_json t))
  | Multi errors -> "multiple errors", Some (`Array (List.map errors ~f:to_json))
  | With_backtrace (t, backtrace) ->
    let message, data = to_message_and_data t in
    message, Some (append_to_data data (backtrace_json backtrace))
;;
