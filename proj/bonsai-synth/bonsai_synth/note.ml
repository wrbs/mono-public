open! Core

module Letter = struct
  type t =
    | C
    | CS [@rename "C#"]
    | D
    | DS [@rename "D#"]
    | E
    | F
    | FS [@rename "F#"]
    | G
    | GS [@rename "G#"]
    | A
    | AS [@rename "A#"]
    | B
  [@@deriving string ~case_insensitive, compare, hash, enumerate]

  include Sexpable.Of_stringable (struct
      type nonrec t = t [@@deriving string]
    end)

  let to_int = function
    | C -> 0
    | CS -> 1
    | D -> 2
    | DS -> 3
    | E -> 4
    | F -> 5
    | FS -> 6
    | G -> 7
    | GS -> 8
    | A -> 9
    | AS -> 10
    | B -> 11
  ;;

  let of_int n =
    match n % 12 with
    | 0 -> C
    | 1 -> CS
    | 2 -> D
    | 3 -> DS
    | 4 -> E
    | 5 -> F
    | 6 -> FS
    | 7 -> G
    | 8 -> GS
    | 9 -> A
    | 10 -> AS
    | 11 -> B
    | _ -> failwith "unreachable"
  ;;

  let is_sharp = function
    | C | D | E | F | G | A | B -> false
    | CS | DS | FS | GS | AS -> true
  ;;
end

type t = Letter.t * int [@@deriving hash]

let compare =
  Comparable.lift [%compare: int * Letter.t] ~f:(fun (letter, octave) -> octave, letter)
;;

let to_string (letter, octave) = [%string "%{letter#Letter}%{octave#Int}"]

let of_string s =
  let letter_len = if String.mem s '#' then 2 else 1 in
  let letter = Letter.of_string (String.prefix s letter_len) in
  let octave = Int.of_string (String.drop_prefix s letter_len) in
  letter, octave
;;

include Sexpable.Of_stringable (struct
    type nonrec t = t [@@deriving string]
  end)

include Comparable.Make (struct
    type nonrec t = t [@@deriving sexp, compare]
  end)

include Hashable.Make (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
  end)

let letter (letter, _) = letter
let octave (_, octave) = octave

let to_int (letter, octave) =
  (* C-1 == 0 *)
  Letter.to_int letter + ((octave + 1) * 12)
;;

let of_int n =
  let octave = (n / 12) - 1 in
  Letter.of_int n, octave
;;

let to_midi_value_exn t = Midi_value.of_int_exn (to_int t)
let of_midi_value m = of_int (Midi_value.to_int m)
let middle_c : t = C, 4
let midi_bottom = of_int 0
let midi_top = of_int 127
let freq_base = to_int (A, 4)

let frequency ?(a = 440.) t =
  let offset = to_int t - freq_base |> Int.to_float in
  a *. (2. **. (offset /. 12.))
;;

let to_aligned_string_exn ((letter, octave) as note) =
  if Int.O.(octave < 0 || octave > 9)
  then
    raise_s
      [%message
        "Note.to_aligned_string_exn: octave out of range"
          (note : t)
          ~range:((0, 9) : int * int)];
  if Letter.is_sharp letter
  then [%string "%{letter#Letter}%{octave#Int}"]
  else [%string "%{letter#Letter}-%{octave#Int}"]
;;

let transpose ?(semis = 0) ?(octaves = 0) t =
  let offset = (octaves * 12) + semis in
  of_int (to_int t + offset)
;;
