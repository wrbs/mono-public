open! Core

module Encoding = struct
  type _ t =
    | Int_clamp : int t
    | Float : float t
    | Float_pm : float t
    | Float_range : (float * float) -> float t
    | Percent : Percent.t t
    | Bool : bool t
end

module T = struct
  module Make_range (M : sig
      val max : int
      val module_name : string
      val uuid : string
    end) : sig
    @@ portable
    type t = private int [@@deriving quickcheck ~portable]

    include Identifiable.S [@mode local] with type t := t

    val to_int : t -> int
    val of_int : int -> t option
    val of_int_exn : int -> t
    val min_value : t
    val max_value : t
    val min_int : int
    val max_int : int
    val encode : 'a Encoding.t -> 'a -> t
    val decode : 'a Encoding.t -> t -> 'a
  end = struct
    type t = int
    [@@deriving to_string, compare ~localize, hash, quickcheck ~observer ~shrinker]

    let min_value = 0
    let max_value = M.max
    let min_int = 0
    let max_int = M.max
    let to_int t = t
    let of_int n = if n >= min_int && n <= max_int then Some n else None

    let of_int_exn value =
      match of_int value with
      | Some t -> t
      | None ->
        raise_s
          [%message
            [%string "%{M.module_name}.of_int_exn: value out of range"]
              (value : int)
              ~range:((min_int, max_int) : int * int)]
    ;;

    let to_string t = Int.to_string t
    let of_string s = Int.of_string s |> of_int_exn

    include functor Sexpable.Of_stringable [@modality portable]

    include
      Binable.Of_binable_with_uuid [@modality portable] [@mode local]
        (Int)
        (struct
          type t = int

          let%template to_binable t = t [@@mode _ = (global, local)]
          let of_binable = of_int_exn
          let caller_identity = Bin_shape.Uuid.of_string M.uuid
        end)

    let module_name = M.module_name

    include functor Identifiable.Make [@modality portable] [@mode local]

    let quickcheck_generator = Int.gen_incl min_int max_int
    let range_size = max_int - min_int |> Float.of_int
    let mid = (max_int + 1) / 2

    let encode (type a) (encoding : a Encoding.t) (x : a) =
      let clamp (x : int) = if x < 0 then 0 else if x > max_value then max_value else x in
      let float f =
        let n = f *. range_size |> Float.round |> Float.to_int in
        clamp (min_int + n)
      in
      match encoding with
      | Int_clamp -> clamp x
      | Float -> float x
      | Float_pm -> float ((x +. 1.) /. 2.)
      | Float_range (low, high) -> float ((x -. low) /. (high -. low))
      | Percent -> Percent.to_mult x |> float
      | Bool -> if x then max_value else min_value
    ;;

    let decode (type a) (encoding : a Encoding.t) (t : t) : a =
      let float t = Float.of_int t /. 127. in
      match encoding with
      | Int_clamp -> t
      | Float -> float t
      | Float_pm -> (float t *. 2.) -. 1.
      | Float_range (low, high) -> (float t *. (high -. low)) +. low
      | Percent -> float t |> Percent.of_mult
      | Bool -> t >= mid
    ;;
  end

  include Make_range (struct
      let max = 0x7F
      let module_name = "Midi.Value"
      let uuid = "d7908373-264f-49f0-858c-e43a0f4ce59e"
    end)

  let all = List.init max_int ~f:of_int_exn
  let to_byte (t : t) = Char.unsafe_of_int (t :> int)
  let of_byte c = of_int (Byte.to_int c)
  let of_byte_exn c = of_int_exn (Byte.to_int c)
end

include T

module Double = struct
  include Make_range (struct
      let max = 0x3FFF
      let module_name = "Midi.Value.Double"
      let uuid = "dc4a18b4-47bb-40bb-937e-c441b891da80"
    end)

  let of_values ~hi ~lo =
    let hi = T.to_int hi in
    let lo = T.to_int lo in
    of_int_exn ((hi lsl 7) lor lo)
  ;;

  let to_values t =
    let n = to_int t in
    let hi = n lsr 7 |> T.of_int_exn in
    let lo = n land 0x7F |> T.of_int_exn in
    ~hi, ~lo
  ;;
end
