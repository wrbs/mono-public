open! Core

type t = float Array.t [@@deriving sexp_of, equal]

let size = 64
let const v : t = Array.create ~len:size v
let zero = const 0.
let make f = Array.init size ~f
let get (t : t) n = t.(n)
let first t = get t 0
let last t = get t (size - 1)
let map t ~f = make (fun idx -> f t.(idx))
let mapi t ~f = make (fun idx -> f idx t.(idx))
let map2 t1 t2 ~f = make (fun idx -> f t1.(idx) t2.(idx))
let mapi2 t1 t2 ~f = make (fun idx -> f idx t1.(idx) t2.(idx))
let map3 t1 t2 t3 ~f = make (fun idx -> f t1.(idx) t2.(idx) t3.(idx))
let mapi3 t1 t2 t3 ~f = make (fun idx -> f idx t1.(idx) t2.(idx) t3.(idx))

let fold t ~init ~f =
  let acc = ref init in
  Array.iter t ~f:(fun x -> acc := f !acc x);
  !acc
;;

let foldi t ~init ~f =
  let acc = ref init in
  Array.iteri t ~f:(fun idx x -> acc := f !acc idx x);
  !acc
;;

let fold_map t ~init ~f =
  let acc = ref init in
  let t' =
    make (fun idx ->
      let acc', x = f !acc t.(idx) in
      acc := acc';
      x)
  in
  !acc, t'
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let t' =
    make (fun idx ->
      let acc', x = f !acc idx t.(idx) in
      acc := acc';
      x)
  in
  !acc, t'
;;

let unfoldi init ~f =
  let acc = ref init in
  let t' =
    make (fun idx ->
      let acc', x = f !acc idx in
      acc := acc';
      x)
  in
  !acc, t'
;;

let unfold init ~f = unfoldi init ~f:(fun state _ -> f state)

module O = struct
  let ( + ) = map2 ~f:Float.( + )
  let ( - ) = map2 ~f:Float.( - )
  let ( * ) = map2 ~f:Float.( * )
  let ( / ) = map2 ~f:Float.( / )
  let ( +. ) t v = map t ~f:(fun x -> Float.(x + v))
  let ( -. ) t v = map t ~f:(fun x -> Float.(x - v))
  let ( *. ) t v = map t ~f:(fun x -> Float.(x * v))
  let ( /. ) t v = map t ~f:(fun x -> Float.(x / v))
end

include O
