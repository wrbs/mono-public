open! Core

module T = struct
  type t = int [@@deriving sexp_of, to_string, compare, hash]

  let to_int t = t
  let low = 0
  let high = 127
  let of_int n = if n >= low && n <= high then Some n else None

  let of_int_exn n =
    match of_int n with
    | Some t -> t
    | None ->
      raise_s
        [%message
          "Midi_value.of_int_exn: value out of range"
            (n : int)
            ~range:((low, high) : int * int)]
  ;;

  let t_of_sexp sexp = of_int_exn ([%of_sexp: int] sexp)
  let of_string s = of_int_exn (Int.of_string s)
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let all = List.init 128 ~f:(fun n -> n)
