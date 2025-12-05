open! Core

module type S = sig @@ portable
  type t = private int [@@deriving quickcheck ~portable]

  include Identifiable.S [@modality portable] [@mode local] with type t := t

  val bits : int
  val to_int : t -> int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val min_value : t
  val max_value : t
  val min_int : int
  val max_int : int
end

module Make
    (M : sig
       val bits : int
     end)
    () =
struct
  type t = int
  [@@deriving
    sexp_of
    , to_string
    , compare ~portable ~localize
    , hash
    , quickcheck ~portable ~observer ~shrinker]

  let bits = M.bits
  let min_value = 0
  let max_value = (1 lsl bits) - 1
  let min_int = 0
  let max_int = max_value
  let to_int t = t
  let of_int n = if n >= min_int && n <= max_int then Some n else None
  let module_name = [%string "Midi_file.Nums.U%{bits#Int}"]

  let of_int_exn value =
    match of_int value with
    | Some t -> t
    | None ->
      raise_s
        [%message
          [%string "%{module_name}.of_int_exn: value out of range"]
            (value : int)
            ~range:((min_int, max_int) : int * int)]
  ;;

  let to_string t = Int.to_string t
  let of_string s = Int.of_string s |> of_int_exn

  include functor Sexpable.Of_stringable [@modality portable]

  module B =
    Binable.Of_binable_with_uuid [@modality portable] [@mode local]
      (Int)
      (struct
        type t = int

        let%template to_binable t = t [@@mode _ = (global, local)]
        let of_binable = of_int_exn

        let caller_identity =
          Bin_shape.Uuid.of_string
            (Printf.sprintf "de7309c5-4b02-4376-82e7-acac09f49a%02d" bits)
        ;;
      end)

  include B
  include functor Identifiable.Make [@modality portable] [@mode local]

  let quickcheck_generator = Int.gen_incl min_int max_int
end

module U15 = struct
  include
    Make
      (struct
        let bits = 15
      end)
      ()
end

module U24 = struct
  include
    Make
      (struct
        let bits = 24
      end)
      ()
end

module U28 = struct
  include
    Make
      (struct
        let bits = 28
      end)
      ()
end
