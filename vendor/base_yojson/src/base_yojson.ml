module Map_yojson = Map_yojson
module Yojson_conv = Ppx_yojson_conv_lib.Yojson_conv
module Yojson_conv_error = Ppx_yojson_conv_lib.Yojson_conv_error
module Yojson = Yojson0
module Yojsonable = Yojsonable
include Yojson_conv.Primitives

let () =
  let module Sexp_conv = Sexplib0.Sexp_conv in
  Sexp_conv.Exn_converter.add
    [%extension_constructor Yojson.Safe.Of_yojson_error]
    (Base.Obj.magic_portable (function
      | Yojson.Safe.Of_yojson_error (v0, v1) ->
        let v0 =
          match v0 with
          | Failure v0 -> Sexplib0.Sexp.Atom v0
          | v0 -> Sexp_conv.sexp_of_exn v0
        and v1 = Yojson.Safe.sexp_of_t v1 in
        Sexplib0.Sexp.(List [ Atom "Of_yojson_error"; v0; v1 ])
      | _ -> assert false))
;;

let () =
  let module Sexp_conv = Sexplib0.Sexp_conv in
  Sexp_conv.Exn_converter.add [%extension_constructor Yojson.Json_error] (function
    | Yojson.Json_error s ->
      let s = Sexp_conv.sexp_of_string s in
      Sexplib0.Sexp.(List [ Atom "Json_error"; s ])
    | _ -> assert false)
;;
