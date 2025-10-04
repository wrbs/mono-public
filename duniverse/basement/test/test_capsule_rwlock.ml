open! Base
open Stdlib
open Basement

(* [Rwlock.t] and [Data.t] are [value mod portable contended]. *)

type 'k _rwlock : value mod contended portable = 'k Capsule.Rwlock.t
type ('a, 'k) _data : value mod contended portable = ('a, 'k) Capsule.Data.t

(* Packed rwlocks are [value mod portable contended]. *)

type _packed : value mod contended portable = Capsule.Rwlock.packed

type 'a myref : value mod portable = { mutable v : 'a }
[@@unsafe_allow_any_mode_crossing "TODO: This can go away once we have with-kinds"]

module RwCell = struct
  type 'a t = Mk : 'k Capsule.Rwlock.t * ('a myref, 'k) Capsule.Data.t -> 'a t

  let create (type a : value mod contended portable) (x : a) : a t =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let p = Capsule.Data.create (fun () -> { v = x }) in
    Mk (m, p)
  ;;

  let read (type a : value mod contended portable) (t : a t) : a =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_read_lock m ~f:(fun password ->
      let read' : a myref @ shared -> a @ contended portable = fun r -> r.v in
      Capsule.Data.extract_shared p ~password ~f:read')
  ;;

  let write (type a : value mod contended portable) (t : a t) (x : a) =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_write_lock m ~f:(fun password ->
      Capsule.Data.iter p ~password ~f:(fun r -> r.v <- x))
  ;;

  let copy (type a : value mod contended portable) (t : a t) : a t =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_read_lock m ~f:(fun password ->
      let p =
        Capsule.Data.map_shared p ~password ~f:(fun r ->
          let v : a = r.v in
          { v })
      in
      Mk (m, p))
  ;;
end

let%expect_test "[RwCell] basics" =
  let ptr = RwCell.create 42 in
  let ptr' = RwCell.copy ptr in
  [%test_result: int] (RwCell.read ptr) ~expect:42;
  RwCell.write ptr 43;
  [%test_result: int] (RwCell.read ptr) ~expect:43;
  [%test_result: int] (RwCell.read ptr') ~expect:42
;;

(** Testing individual capsule operations over a password captured by a rwlock *)

type 'a guarded = Mk : 'k Capsule.Rwlock.t * ('a, 'k) Capsule.Data.t -> 'a guarded

type ('a, 'b) func =
  { f : 'k. 'k Capsule.Password.t @ local -> ('a, 'k) Capsule.Data.t -> 'b }

let with_write_guarded x (f : ('a, 'b) func) =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_write_lock m ~f:(fun k -> f.f k p)
;;

type ('a, 'b) func' =
  { f : 'k. 'k Capsule.Password.Shared.t @ local -> ('a, 'k) Capsule.Data.t -> 'b }

let with_read_guarded x (f : ('a, 'b) func') =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_read_lock m ~f:(fun k -> f.f k p)
;;

(* reading from myref with the expected modes *)
let read_ref
  : ('a : value mod portable).
  ('a myref @ shared -> 'a aliased @ contended portable unique) @ portable
  =
  fun r -> { aliased = r.v }
;;

(* writing to myref with the expected modes *)
let write_ref : ('a : value mod contended portable). 'a -> ('a myref -> unit) @ portable =
  fun v r -> r.v <- v
;;

type lost_capsule = |

let%expect_test "rwlock API" =
  (* [create]. *)
  let ptr =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    Mk (m, Capsule.Data.create (fun () -> { v = 42 }))
  in
  (* [extract]. *)
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract p ~password ~f:read_ref).aliased
              ~expect:42)
      }
  in
  let ptr' =
    let (Mk (m, _p)) = ptr in
    Mk (m, Capsule.Data.create (fun () -> { v = 2 }))
  in
  (* [iter]. *)
  let () =
    with_write_guarded
      ptr
      { f = (fun password p -> Capsule.Data.iter p ~password ~f:(write_ref 15)) }
  in
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract p ~password ~f:read_ref).aliased
              ~expect:15)
      }
  in
  let () =
    with_write_guarded
      ptr'
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract p ~password ~f:read_ref).aliased
              ~expect:2)
      }
  in
  (* [extract_shared]. *)
  let () =
    with_read_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared p ~password ~f:read_ref).aliased
              ~expect:15)
      }
  in
  (* [map_shared]. *)
  let ptr2 =
    let (Mk (m, p)) = ptr in
    let p' =
      Capsule.Rwlock.with_read_lock m ~f:(fun password ->
        Capsule.Data.map_shared p ~password ~f:(fun (r @ shared) ->
          let v : int = r.v in
          { v = v + 2 }))
    in
    Mk (m, p')
  in
  (* [map_shared] and [extract_shared]. *)
  let () =
    with_read_guarded
      ptr2
      { f =
          (fun password p ->
            let ptr' =
              Capsule.Data.map_shared p ~password ~f:(fun (r @ shared) -> { v = r.v + 3 })
            in
            [%test_result: int]
              (Capsule.Data.extract_shared ptr' ~password ~f:read_ref).aliased
              ~expect:20)
      }
  in
  (* Using a Password.t as a Password.Shared.t *)
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared
                 p
                 ~password:(Capsule.Password.shared password)
                 ~f:read_ref)
                .aliased
              ~expect:15)
      }
  in
  (* [access_shared] and [unwrap_shared]. *)
  let () =
    with_read_guarded
      ptr2
      { f =
          (fun password p ->
            Capsule.access_shared ~password ~f:(fun access ->
              let r = Capsule.Data.unwrap_shared ~access p in
              let ptr' = Capsule.Data.Shared.wrap ~access { v = r.v + 3 } in
              let { aliased = res } =
                Capsule.Data.Shared.extract ptr' ~password ~f:read_ref
              in
              assert (res = 20))
            [@nontail])
      }
  in
  (* [map], [both]. *)
  let ptr2 =
    let (Mk (m, p)) = ptr in
    let p' =
      Capsule.Rwlock.with_write_lock m ~f:(fun password ->
        Capsule.Data.map p ~password ~f:(fun _ -> { v = 3 }))
    in
    Mk (m, Capsule.Data.both p p')
  in
  (* [destroy]. *)
  let () =
    let (Mk (m, p)) = ptr2 in
    let access = Capsule.Key.destroy (Capsule.Rwlock.destroy m) in
    let r1, r2 = Capsule.Data.unwrap ~access p in
    [%test_result: int] (read_ref r1).aliased ~expect:15;
    [%test_result: int] (read_ref r2).aliased ~expect:3
  in
  let () =
    match with_write_guarded ptr { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  let () =
    match with_write_guarded ptr' { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  let () =
    match with_write_guarded ptr2 { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  (* [inject], [project]. *)
  let () =
    let ptr = Capsule.Data.inject 100 in
    [%test_result: int] (Capsule.Data.project ptr) ~expect:100
  in
  (* [bind]. *)
  let ptr' : (int, lost_capsule) Capsule.Data.t =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let ptr = Capsule.Data.inject 100 in
    Capsule.Rwlock.with_write_lock m ~f:(fun password ->
      Capsule.Data.bind ptr ~password ~f:(fun x -> Capsule.Data.inject ((( + ) x) 11)))
  in
  let () = [%test_result: int] (Capsule.Data.project ptr') ~expect:111 in
  (* [freeze]. *)
  let () =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let data = Capsule.Data.create (fun () -> { v = 42 }) in
    with_write_guarded
      (Mk (m, data))
      { f = (fun password p -> Capsule.Data.iter p ~password ~f:(fun r -> r.v <- 999)) };
    let _freeze_key = Capsule.Rwlock.freeze m in
    with_read_guarded
      (Mk (m, data))
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared p ~password ~f:(fun r -> r.v))
              ~expect:999)
      };
    match
      with_write_guarded
        (Mk (m, data))
        { f = (fun password p -> Capsule.Data.iter p ~password ~f:(fun r -> r.v <- 123)) }
    with
    | exception Capsule.Rwlock.Frozen -> ()
    | _ -> assert false
  in
  ()
;;
