open! Core

module Parse = struct
  open Unstring.Let_syntax
  open Let_syntax.Open_on_rhs

  let is_ident_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  ;;

  let is_ident_rest = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  ;;

  let space_or_tab =
    char
      (Fun
         (function
           | ' ' | '\t' -> true
           | _ -> false))
  ;;

  let ignore_spaces_before inner =
    let%map () = star space_or_tab
    and x = inner in
    x
  ;;

  let ignore_spaces_around inner =
    let%map () = star space_or_tab
    and x = inner
    and () = star space_or_tab in
    x
  ;;

  let local_label =
    let%map s = capture (Unstring.all_unit [ char Digit; star (char Digit) ]) in
    Int.of_string s
  ;;

  let digits_parser ~digit ~base =
    Unstring.fold
      (capture'
         (char (Fun (fun c -> Or_null.is_this (digit c))))
         ~f:(fun () ~input ~pos ~len:_ ->
           Or_null.value_exn (digit (String.nget input pos))))
      ~sep:(choice [ return (); char (Char '_') ])
      ~bounds:(Incl 1, Unbounded)
      ~init:0
      ~f:(fun acc digit ->
        (* TODO: overflow if every generalizing *)
        (acc * base) + digit)
  ;;

  let constant =
    let numeric_digits_parser base =
      digits_parser
        ~digit:(function
          | '0' .. '9' as c ->
            let n = Char.to_int c - Char.to_int '0' in
            if n < base then This n else Null
          | _ -> Null)
        ~base
    in
    let with_prefix c rest =
      let%map () = char (Char '0')
      and () = char (Char c)
      and x = rest in
      x
    in
    let decimal = numeric_digits_parser 10 in
    let octal = numeric_digits_parser 8 |> with_prefix 'o' in
    let binary = numeric_digits_parser 2 |> with_prefix 'b' in
    let hex =
      digits_parser
        ~digit:(function
          | '0' .. '9' as c -> This (Char.to_int c - Char.to_int '0')
          | 'a' .. 'f' as c -> This (Char.to_int c - Char.to_int 'a' + 10)
          | 'A' .. 'F' as c -> This (Char.to_int c - Char.to_int 'A' + 10)
          | _ -> Null)
        ~base:16
      |> with_prefix 'x'
    in
    let%map sign = capture (choice [ lit "-"; return () ])
    and rest = choice [ decimal; octal; binary; hex ] in
    match sign with
    | "" -> rest
    | _ -> -rest
  ;;
end

module Register = struct
  include
    (val Unstring_id.make
           ~module_name:__MODULE__
           ~caller_identity:
             (Bin_shape.Uuid.of_string "87f1d64a-9554-4f41-99ee-923fd30e155d")
           (let open Unstring in
            all_unit
              [ char (Char '%')
              ; char (Fun Parse.is_ident_rest)
              ; star (char (Fun Parse.is_ident_rest))
              ]))
end

module Label = struct
  module Name = struct
    include
      (val Unstring_id.make
             ~module_name:__MODULE__
             ~caller_identity:
               (Bin_shape.Uuid.of_string "87f1d64a-9554-4f41-99ee-923fd30e155d")
             (let open Unstring in
              all_unit
                [ char (Fun Parse.is_ident_start); star (char (Fun Parse.is_ident_rest)) ]))
  end

  module Dir = struct
    type t =
      | Forwards [@format "f"]
      | Backwards [@format "b"]
    [@@deriving unstring, sexp_of]
  end

  module Ref = struct
    type t =
      | Global of Name.t [@format "%{_}"]
      | Local of (int[@custom Parse.local_label]) * Dir.t [@format "%{1}%{2}"]
    [@@deriving unstring, sexp_of]
  end

  module Def = struct
    type t =
      | Global of Name.t [@format ":%{_}"]
      | Local of (int[@custom Parse.local_label]) [@format ".%{_}"]
    [@@deriving unstring, sexp_of]
  end
end

(* syntax

   :label .1 MOV thing, thing, thing DAT thing, thing ADD sub, 2 ADD thing, 100, test ADD
   %3, 12, %0
*)

module Operand = struct
  type t =
    | Register of Register.t [@format "%{_}"]
    | Label of Label.Ref.t [@format "%{_}"]
    | Constant of (int[@custom Parse.constant]) [@format "%{_}"]
  [@@deriving unstring, sexp_of]
end

module Opcode : sig
  type t [@@deriving unstring]

  include Identifiable.S with type t := t
end = struct
  type t = Lowercase of string [@@unboxed] [@@deriving compare ~localize, hash]

  let unstring =
    let%map_open.Unstring s =
      capture (multiple (char (Fun Parse.is_ident_start)) ~bounds:(Incl 1, Unbounded))
    in
    Lowercase (String.lowercase s)
  ;;

  let of_string =
    unstage
      (Unstring.make_of_string unstring ~compile_when:`Lazily ~on_ambiguity:`Pick_first)
  ;;

  let to_string (Lowercase s) = s

  include functor Sexpable.Of_stringable

  let caller_identity = Bin_shape.Uuid.of_string "c1986515-a7d0-4d9f-a5b0-8790cc0149ee"

  include functor Binable.Of_stringable_with_uuid

  let module_name = "Opcode"

  include functor Identifiable.Make
end

module Parsed_instruction = struct
  type t =
    { opcode : Opcode.t
    ; operands : Operand.t list [@sexp.list]
    }
  [@@deriving sexp_of]

  let unstring =
    let operands =
      let%map_open.Unstring () = Parse.space_or_tab
      and () = star Parse.space_or_tab
      and x =
        list
          Operand.unstring
          ~sep:(Parse.ignore_spaces_around (char (Char ',')))
          ~bounds:(Incl 1, Unbounded)
      in
      x
    in
    let%map_open.Unstring opcode = Opcode.unstring
    and operands = option operands ~none:(Unstring.return ()) in
    let operands = Option.value operands ~default:[] in
    { opcode; operands }
  ;;
end

module Statement = struct
  type t =
    | Label_def of Label.Def.t [@format "%{_}"]
    | Instruction of Parsed_instruction.t [@format "%{_}"]
  [@@deriving unstring, sexp_of]
end

let unstring_statements =
  let open Unstring in
  let line_end =
    all_unit
      [ star Parse.space_or_tab
      ; choice
          [ all_unit
              [ char (Char ';')
              ; star
                  (char
                     (Fun
                        (function
                          | '\r' | '\n' -> false
                          | _ -> true)))
              ]
          ; return ()
          ]
      ]
  in
  let newline = choice [ lit "\r\n"; lit "\n" ] in
  let line =
    option (Parse.ignore_spaces_before Statement.unstring) ~none:(return ()) <* line_end
  in
  let%map.Unstring lines_rev =
    fold
      line
      ~sep:newline
      ~init:[]
      ~f:(fun acc l ->
        match l with
        | None -> acc
        | Some t -> t :: acc)
      ~bounds:(Incl 1, Unbounded)
  in
  List.rev lines_rev
;;

let parse =
  unstage
    (Unstring.make_of_string
       unstring_statements
       ~compile_when:`Lazily
       ~on_ambiguity:(`Raise [%sexp_of: Statement.t list]))
;;

module For_testing = struct
  let print_shape () = Unstring.print_shape unstring_statements
end
