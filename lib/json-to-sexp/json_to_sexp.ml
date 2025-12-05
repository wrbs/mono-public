open! Core

let rec convert : Jsonaf.t -> Sexp.t = function
  | `Null -> Atom "null"
  | `False -> Atom "false"
  | `True -> Atom "true"
  | `String s -> Atom s
  | `Number s -> Atom s
  | `Object alist ->
    List (List.map alist ~f:(fun (key, value) -> Sexp.List [ Atom key; convert value ]))
  | `Array xs -> List (List.map xs ~f:convert)
;;
