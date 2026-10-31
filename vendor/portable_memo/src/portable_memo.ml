open! Core
include Portable_memo_intf.Definitions
module Domain = Basement.Stdlib_shim.Domain
module Backoff = Basement.Stdlib_shim.Backoff

type ('a, 'b) t : value mod contended portable =
  | T :
      ('a : value mod contended) ('b : value mod contended) ('cw : value mod portable).
      { per_thread_cache :
          ('a Modes.Portable.t, 'b Modes.Portable.t) Hashtbl.t Modes.Portable.t
            Domain.Safe.TLS.key
      ; shared_cache : ('a Modes.Portable.t, 'b Portable_lazy.t, 'cw) Map.t Atomic.t
        @@ portable
      }
      -> ('a, 'b) t

let create
  (type (k : value mod contended) (v : value mod contended) (cw : value mod portable))
  ((module T) : ((k, cw) Comparator_hashable.t[@modality portable]))
  =
  let (module Portable_comparator) =
    (Comparator.make [@mode portable])
      ~compare:(fun t1 t2 -> T.compare t1.portable t2.portable)
      ~sexp_of_t:(fun t -> T.sexp_of_t t.portable)
  in
  let per_thread_cache =
    Domain.Safe.TLS.new_key (fun () ->
      { portable =
          (Hashtbl.create [@mode portable])
            ((Hashtbl.Hashable.to_key [@mode portable])
               { hash = (fun x -> T.hashable.hash x.portable)
               ; compare = (fun x1 x2 -> T.hashable.compare x1.portable x2.portable)
               ; sexp_of_t = (fun x -> T.hashable.sexp_of_t x.portable)
               })
      })
  in
  let shared_cache : (k Modes.Portable.t, v Portable_lazy.t, _) Map.t Atomic.t =
    Atomic.make
      ((Base.Map.empty [@mode portable])
         (module struct
           type t = k Modes.Portable.t

           include Portable_comparator
         end))
  in
  T { per_thread_cache; shared_cache }
;;

let find_or_null (type a b) (T { per_thread_cache; shared_cache } : (a, b) t) (key : a)
  : b or_null
  =
  let per_thread_cache =
    Obj.magic_uncontended (Domain.Safe.TLS.get per_thread_cache).portable
  in
  let key = Modes.Portable.wrap key in
  match Hashtbl.find_or_null per_thread_cache key with
  | This _ as some_data -> Modes.Portable.unwrap_or_null some_data
  | Null ->
    (match Map.find (Atomic.get shared_cache) key with
     | None -> Null
     | Some data ->
       let data = Portable_lazy.force data in
       Hashtbl.set per_thread_cache ~key ~data:(Modes.Portable.wrap data);
       This data)
;;

let find t key =
  match find_or_null t key with
  | This x -> Some x
  | Null -> None
;;

let ensure_added_to_shared_cache
  : type (a : value mod contended) (b : value mod contended) (cw : value mod portable).
    shared_cache:(a Modes.Portable.t, b Portable_lazy.t, cw) Map.t Atomic.t
    -> key:a @ portable
    -> data:b Portable_lazy.t
    -> b @ portable
  =
  fun ~shared_cache ~key ~data ->
  let[@inline] rec loop backoff =
    let old_shared_cache = Atomic.get shared_cache in
    match Map.add old_shared_cache ~key:(Modes.Portable.wrap key) ~data with
    | `Ok new_shared_cache ->
      (match
         Atomic.compare_and_set
           shared_cache
           ~if_phys_equal_to:old_shared_cache
           ~replace_with:new_shared_cache
       with
       | Set_here -> Portable_lazy.force data
       | Compare_failed -> loop (Backoff.once backoff))
    | `Duplicate ->
      let data = Map.find_exn old_shared_cache (Modes.Portable.wrap key) in
      Portable_lazy.force data
  in
  loop Backoff.default
;;

let ensure_added_to_all_caches
  (type a b)
  (T { per_thread_cache; shared_cache } : (a, b) t)
  ~(key : a)
  ~(data : b Portable_lazy.t)
  : b
  =
  let data = ensure_added_to_shared_cache ~shared_cache ~key ~data in
  let per_thread_cache =
    Obj.magic_uncontended (Domain.Safe.TLS.get per_thread_cache).portable
  in
  Hashtbl.add
    per_thread_cache
    ~key:(Modes.Portable.wrap key)
    ~data:(Modes.Portable.wrap data)
  |> (ignore : [ `Duplicate | `Ok ] -> unit);
  data
;;

let add t ~key ~data =
  ensure_added_to_all_caches t ~key ~data:(Portable_lazy.from_val data)
;;

let memo c f =
  let t = create c in
  fun key ->
    match find_or_null t key with
    | Null ->
      ensure_added_to_all_caches t ~key ~data:(Portable_lazy.from_fun (fun () -> f key))
    | This data -> data
;;

(* Pull everything currently in the shared cache into the local cache. *)
let sync_to_local_cache (type a b) (T t : (a, b) t) : unit =
  let shared_cache = Atomic.get t.shared_cache in
  let per_thread_cache =
    Obj.magic_uncontended (Domain.Safe.TLS.get t.per_thread_cache).portable
  in
  Map.iteri shared_cache ~f:(fun ~key ~data ->
    Hashtbl.add
      per_thread_cache
      ~key
      ~data:(Modes.Portable.wrap (Portable_lazy.force data))
    |> (ignore : [ `Duplicate | `Ok ] -> unit))
;;

let to_alist (type a b) (T t : (a, b) t) : (a * b) list =
  sync_to_local_cache (T t);
  let per_thread_cache =
    Obj.magic_uncontended (Domain.Safe.TLS.get t.per_thread_cache).portable
  in
  Hashtbl.to_alist per_thread_cache
  |> List.map ~f:(fun (k, v) -> (k.portable, v.portable) |> Modes.Portable.wrap)
  |> Modes.Portable.unwrap_list
;;

module Templated = struct
  let%template memo = memo [@@mode portable]

  let memo (type a cw) ((module C) : (a, cw) Comparator_hashable.t) f =
    Memo.general ~hashable:C.hashable f
  ;;
end
