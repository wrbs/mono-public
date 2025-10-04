(* We implement a weak pointer using a [Weak_array.t]. *)

open! Base

type 'a t = 'a Weak_array.t

let create () = Weak_array.create ~len:1

(* We use a weak array of length 1, so the weak pointer is at index 0. *)
let index = 0
let get_as_heap_block t = Weak_array.get t index
let get t = get_as_heap_block t |> Option.map ~f:Heap_block.value
let sexp_of_t sexp_of_a t = [%sexp (get t : a option)]
let is_none t = Weak_array.is_none t index
let is_some t = Weak_array.is_some t index
let set t block = Weak_array.set t index (Some block)

let create_full block =
  let t = create () in
  set t block;
  t
;;
