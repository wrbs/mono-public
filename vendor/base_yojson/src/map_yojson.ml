open! Base
include Map_yojson_intf.Definitions

let yojson_of_m__t (type k) (module K : Yojson_of_m with type t = k) yojson_of_v t =
  `List
    (Base.List.map (Base.Map.to_alist t) ~f:(fun (k, v) ->
       `List [ K.yojson_of_t k; yojson_of_v v ]))
;;

let m__t_of_yojson
  (type k cmp)
  (module K : M_of_yojson with type t = k and type comparator_witness = cmp)
  v_of_yojson
  yojson
  =
  match (yojson : Yojson.Safe.t) with
  | `List lst ->
    let pairs =
      Base.List.map lst ~f:(function
        | `List [ k_yojson; v_yojson ] -> K.t_of_yojson k_yojson, v_of_yojson v_yojson
        | _ ->
          Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
            "Map.m__t_of_yojson: expected a list of 2-element lists"
            yojson)
    in
    (match Base.Map.of_alist_or_error (module K) pairs with
     | Ok map -> map
     | Error error ->
       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
         (Printf.sprintf "Map.m__t_of_yojson: %s" (Error.to_string_hum error))
         yojson)
  | _ ->
    Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
      "Map.m__t_of_yojson: expected a list of pairs"
      yojson
;;
