open! Core

type t =
  | C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | C11
  | C12
  | C13
  | C14
  | C15
  | C16
[@@deriving quickcheck ~portable, enumerate, compare ~localize, hash, bin_io ~localize]

let to_int = function
  | C1 -> 1
  | C2 -> 2
  | C3 -> 3
  | C4 -> 4
  | C5 -> 5
  | C6 -> 6
  | C7 -> 7
  | C8 -> 8
  | C9 -> 9
  | C10 -> 10
  | C11 -> 11
  | C12 -> 12
  | C13 -> 13
  | C14 -> 14
  | C15 -> 15
  | C16 -> 16
;;

let of_int = function
  | 1 -> Some C1
  | 2 -> Some C2
  | 3 -> Some C3
  | 4 -> Some C4
  | 5 -> Some C5
  | 6 -> Some C6
  | 7 -> Some C7
  | 8 -> Some C8
  | 9 -> Some C9
  | 10 -> Some C10
  | 11 -> Some C11
  | 12 -> Some C12
  | 13 -> Some C13
  | 14 -> Some C14
  | 15 -> Some C15
  | 16 -> Some C16
  | _ -> None
;;

let of_int_exn = function
  | 1 -> C1
  | 2 -> C2
  | 3 -> C3
  | 4 -> C4
  | 5 -> C5
  | 6 -> C6
  | 7 -> C7
  | 8 -> C8
  | 9 -> C9
  | 10 -> C10
  | 11 -> C11
  | 12 -> C12
  | 13 -> C13
  | 14 -> C14
  | 15 -> C15
  | 16 -> C16
  | n ->
    raise_s
      [%message
        "Midi.Channel.of_int_exn: out of range" (n : int) ~range:((1, 16) : int * int)]
;;

include
  Sexpable.Of_sexpable [@modality portable]
    (Int)
    (struct
      type nonrec t = t

      let to_sexpable = to_int
      let of_sexpable = of_int_exn
    end)

let to_string t = t |> to_int |> Int.to_string
let of_string s = s |> Int.of_string |> of_int_exn
let module_name = "Midi.Channel"

include functor Identifiable.Make [@modality portable] [@mode local]

let next_wrap = function
  | C1 -> C2
  | C2 -> C3
  | C3 -> C4
  | C4 -> C5
  | C5 -> C6
  | C6 -> C7
  | C7 -> C8
  | C8 -> C9
  | C9 -> C10
  | C10 -> C11
  | C11 -> C12
  | C12 -> C13
  | C13 -> C14
  | C14 -> C15
  | C15 -> C16
  | C16 -> C1
;;

let prev_wrap = function
  | C1 -> C16
  | C2 -> C1
  | C3 -> C2
  | C4 -> C3
  | C5 -> C4
  | C6 -> C5
  | C7 -> C6
  | C8 -> C7
  | C9 -> C8
  | C10 -> C9
  | C11 -> C10
  | C12 -> C11
  | C13 -> C12
  | C14 -> C13
  | C15 -> C14
  | C16 -> C15
;;

let of_lower_bits byte = (Byte.to_int byte land 0xF) + 1 |> of_int_exn
let to_lower_bits t = to_int t - 1 |> Byte.of_int_exn

let arg_type =
  (Command.Arg_type.create_with_additional_documentation [@modality portable])
    of_string
    ~additional_documentation:(lazy "range 1-16")
;;
