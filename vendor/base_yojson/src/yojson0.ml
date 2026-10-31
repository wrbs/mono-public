module Safe = struct
  include Yojson.Safe

  let compare : t -> t -> int = Base.Poly.compare
  let compare__local : t @ local -> t @ local -> int = Base.Poly.compare
  let equal : t -> t -> bool = Base.Poly.equal
  let equal__local : t @ local -> t @ local -> bool = Base.Poly.equal
  let sexp_of_t s = Sexplib0.Sexp.Atom (to_string s)
  let yojson_of_t t = t
  let t_of_yojson t = t
  let of_string x = from_string x

  module Alternate_sexp = struct
    open Base

    type t =
      [ `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `String of string
      | `Assoc of (string * t) list
      | `List of t list
      | `Tuple of t list
      | `Variant of string * t option
      ]
      constraint t = Yojson.Safe.t
    [@@deriving sexp_of]
  end

  exception Of_yojson_error = Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error

  let () =
    let module Sexp_conv = Sexplib0.Sexp_conv in
    Sexp_conv.Exn_converter.add
      [%extension_constructor Of_yojson_error]
      (Base.Obj.magic_portable (function
        | Of_yojson_error (v0, v1) ->
          let v0 = Sexp_conv.sexp_of_exn v0
          and v1 = sexp_of_t v1 in
          Sexplib0.Sexp.(List [ Atom "Of_yojson_error"; v0; v1 ])
        | _ -> assert false))
  ;;
end

module Basic = struct
  include Yojson.Basic

  let compare : t -> t -> int = Base.Poly.compare
  let equal : t -> t -> bool = Base.Poly.equal
  let sexp_of_t s = Sexplib0.Sexp.Atom (to_string s)
  let yojson_of_t t = t
  let t_of_yojson t = t
  let of_string = from_string
end

exception Json_error = Yojson.Json_error

let () =
  let module Sexp_conv = Sexplib0.Sexp_conv in
  Sexp_conv.Exn_converter.add [%extension_constructor Json_error] (function
    | Json_error s ->
      let s = Sexp_conv.sexp_of_string s in
      Sexplib0.Sexp.(List [ Atom "Json_error"; s ])
    | _ -> assert false)
;;

let json_error = Yojson.json_error

include Safe
