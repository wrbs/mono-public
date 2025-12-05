open! Core

module T = struct
  type t = Char.t
  [@@deriving bin_io ~localize, compare ~localize, hash, quickcheck, enumerate]

  let to_string t = Printf.sprintf "%02X" (Char.to_int t)

  let of_string s =
    match Scanf.sscanf_opt s "%X" Char.of_int with
    | Some (Some t) -> t
    | None | Some None ->
      raise_s [%message "Byte.of_string: expected 2 hex digits" (s : string)]
  ;;

  include functor Sexpable.Of_stringable [@modality portable]

  let module_name = "Byte"

  include functor Identifiable.Make [@modality portable] [@mode local]

  external of_char : char -> t @@ portable = "%identity"
  external to_char : t -> char @@ portable = "%identity"

  let to_int = Char.to_int
  let of_int = Char.of_int
  let of_int_exn = Char.of_int_exn
  let of_int_wrap n = Char.unsafe_of_int (n land 0xFF)
  let min_value = Char.min_value
  let max_value = Char.max_value

  let range_incl' t t' =
    let a = to_int t in
    let b = to_int t' in
    let len = abs (b - a) + 1 in
    let start = Int.min a b in
    ~len, ~f:(fun i -> of_int_exn (start + i))
  ;;

  let range_incl_list t t' =
    let ~len, ~f = range_incl' t t' in
    List.init len ~f
  ;;

  let range_incl_array t t' =
    let ~len, ~f = range_incl' t t' in
    Array.init len ~f
  ;;

  let range_incl_iarray t t' =
    let ~len, ~f = range_incl' t t' in
    Iarray.init len ~f
  ;;

  module O = struct
    include (
    struct
      let equal = equal
      let compare = compare
      let min = min
      let max = max
      let ( = ) = ( = )
      let ( <> ) = ( <> )
      let ( > ) = ( > )
      let ( < ) = ( < )
      let ( >= ) = ( >= )
      let ( <= ) = ( <= )
    end :
      Comparisons.S with type t := t)

    let ( + ) t t' = of_int_wrap (to_int t + to_int t')
    let ( - ) t t' = of_int_wrap (to_int t - to_int t')
    let ( lsl ) t n = of_int_wrap (to_int t lsl n)
    let ( lsr ) t n = Char.unsafe_of_int (to_int t lsr n)
    let lnot t = of_int_wrap (lnot (to_int t))
    let ( land ) t t' = Char.unsafe_of_int (to_int t land to_int t')
    let ( lor ) t t' = Char.unsafe_of_int (to_int t lor to_int t')
    let ( lxor ) t t' = Char.unsafe_of_int (to_int t lxor to_int t')
  end

  include O
end

include T

module String = struct
  type t = String.t
  [@@deriving string, bin_io ~localize, compare ~localize, hash, quickcheck]

  include
    Sexpable.Of_sexpable [@modality portable]
      (struct
        type nonrec t = T.t list [@@deriving sexp]
      end)
      (struct
        type nonrec t = t

        let to_sexpable s = String.to_list s
        let of_sexpable l = String.of_char_list l
      end)

  let module_name = "Byte.String"

  include functor Identifiable.Make [@modality portable] [@mode local]
end
