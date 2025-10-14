type 'a t = 'a or_null Ref.t

let[@inline] make v = Ref.make (This v)
let[@inline] get_or_null t = Ref.exchange t Null

let[@inline] get_exn t =
  match Ref.exchange t Null with
  | Null -> Basement.Stdlib_shim.failwith "Once.get_exn failed: already accessed"
  | This v -> v
;;

module Atomic = struct
  type 'a t = 'a option Atomic.t

  let[@inline] make v = Atomic.make (Some v)
  let[@inline] get_opt t = Atomic.exchange t None

  let[@inline] get_exn t =
    match Atomic.exchange t None with
    | None -> Basement.Stdlib_shim.failwith "Once.get_exn failed: already accessed"
    | Some v -> v
  ;;
end
