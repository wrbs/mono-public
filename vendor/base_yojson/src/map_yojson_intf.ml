open! Base

module Definitions = struct
  module type Yojson_of_m = sig
    type t

    val yojson_of_t : t -> Yojson.Safe.t
  end

  module type M_of_yojson = sig
    type t

    val t_of_yojson : Yojson.Safe.t -> t

    include Comparator.S with type t := t
  end
end

module type Map_yojson = sig
  (** Yojson converters for [Map.M(Key).t]:
      {[
        module Map = struct
          include Map
          include Base_yojson.Map_yojson
        end

        module Int = struct
          include Int

          let yojson_of_t = yojson_of_int
          let t_of_yojson = int_of_yojson
        end

        type t = string Map.M(Int).t [@@deriving yojson]
      ]} *)

  include module type of struct
    include Definitions
  end

  val yojson_of_m__t
    :  (module Yojson_of_m with type t = 'k)
    -> ('v -> Yojson.Safe.t)
    -> ('k, 'v, _) Map.t
    -> Yojson.Safe.t

  val m__t_of_yojson
    :  (module M_of_yojson with type t = 'k and type comparator_witness = 'cmp)
    -> (Yojson.Safe.t -> 'v)
    -> Yojson.Safe.t
    -> ('k, 'v, 'cmp) Map.t
end
