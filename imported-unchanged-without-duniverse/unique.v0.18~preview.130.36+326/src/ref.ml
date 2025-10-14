type (!'a : value_or_null) t : mutable_data with 'a

external make
  : ('a : value_or_null).
  'a @ once unique -> ('a t[@local_opt]) @ unique
  @@ portable
  = "%makemutable"

external get
  : ('a : value_or_null).
  ('a t[@local_opt]) @ unique -> 'a @ once unique
  @@ portable
  = "%field0"

external set
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ once unique -> unit
  @@ portable
  = "%setfield0"

let[@inline] exchange t a =
  let a' = get (Basement.Stdlib_shim.Obj.magic_unique t) in
  set t a;
  a'
;;
