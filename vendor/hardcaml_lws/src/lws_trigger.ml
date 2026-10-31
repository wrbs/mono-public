open Core

type t =
  { created_at : Source_code_position.t
  ; name : string
  ; unique_id : int
  }
[@@deriving sexp_of]

let running_counter = ref (-1)

let create ~(created_at : [%call_pos]) name =
  Int.incr running_counter;
  { created_at; name; unique_id = !running_counter }
;;

let is_equal a b = Int.equal a.unique_id b.unique_id
