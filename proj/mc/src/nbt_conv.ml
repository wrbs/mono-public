open! Core
module Hlist = Heterogeneous_list

type ('a, 'b) t2 =
  | Id : (Nbt.t, Nbt.t) t2
  | Map : ('a, 'b) t2 * ('b -> 'c) -> ('a, 'c) t2
  | Unmap : ('a -> 'b) * ('b, 'c) t2 -> ('a, 'c) t2
  | Lazy : ('a, 'b) t2 Lazy.t -> ('a, 'b) t2

type 'a t = ('a, 'a) t2

let parse t (nbt : Nbt.t) =
  let rec aux : type a b. (a, b) t2 -> b = function
    | Id -> nbt
    | Map (t, f) -> f (aux t)
    | Unmap (_, t) -> aux t
    | Lazy l -> aux (Lazy.force l)
  in
  aux t
;;

let rec dump : type a b. (a, b) t2 -> a -> Nbt.t =
  fun t data ->
  match t with
  | Id -> data
  | Map (t, _) -> (dump t) data
  | Unmap (f, t) -> dump t (f data)
  | Lazy l -> dump (Lazy.force l) data
;;

let map x ~f = Map (x, f)
let unmap x ~f = Unmap (f, x)
let ( >>| ) x f = map x ~f
let ( <<| ) x f = unmap x ~f

let expect_type (type a) (expected : a Nbt.type_) (nbt : Nbt.t) : a =
  let (T (actual, data)) = nbt in
  match Nbt.is_same_type expected actual with
  | None ->
    raise_s
      [%message
        "Unexpected type" (expected : _ Nbt.type_) (actual : _ Nbt.type_) (nbt : Nbt.t)]
  | Some T -> data
;;

let primitive type_ = Id >>| expect_type type_ <<| fun data -> T (type_, data)
let i8' = primitive Byte
let i16' = primitive Short
let i32' = primitive Int
let i64' = primitive Long
let i8 : int t = i8' <<| Int_repr.Int8.of_base_int_exn >>| Int_repr.Int8.to_base_int
let i16 : int t = i16' <<| Int_repr.Int16.of_base_int_exn >>| Int_repr.Int16.to_base_int

let i32 : int t =
  i32'
  <<| Int_repr.Int32.of_base_int32
  >>| Int_repr.Int32.to_base_int32
  <<| Int32.of_int_exn
  >>| Int32.to_int_exn
;;

let i64 : int t = i64' <<| Int64.of_int_exn >>| Int64.to_int_exn

let wrap_as_list : Nbt.t iarray -> Nbt.nbt_list = function
  | [::] -> Empty_using_end_tag
  | values ->
    let (T (first_type, first_data)) = values.:(0) in
    let payload =
      Iarray.init (Iarray.length values) ~f:(function
        | 0 -> first_data
        | idx ->
          let (T (type_, data)) = values.:(idx) in
          (match Nbt.is_same_type first_type type_ with
           | Some T -> data
           | None ->
             raise_s
               [%message
                 "Expected everything in list to have same nbt type"
                   (first_type : _ Nbt.type_)
                   (type_ : _ Nbt.type_)
                   (idx : int)]))
    in
    Values (first_type, payload)
;;

let list_as_iarray base =
  primitive List
  >>| (function
         | Empty_using_end_tag -> [::]
         | Values (type_, payload) ->
           Iarray.map payload ~f:(fun x -> parse base (T (type_, x))))
  <<| wrap_as_list
  <<| Iarray.map ~f:(dump base)
;;

let list base = list_as_iarray base >>| Iarray.to_list <<| Iarray.of_list
let utf8 = primitive String
let string = utf8 >>| String.Utf8.to_string <<| String.Utf8.of_string

let stringable (type a) (module M : Stringable.S with type t = a) : a t =
  string >>| M.of_string <<| M.to_string
;;

module Compound = struct
  module Parse_state = struct
    type t =
      { data : (int * Nbt.t) Map.M(String).t
      ; handled : Set.M(String).t
      }

    let sexp_of_t t =
      let data =
        Map.to_alist t.data
        |> List.sort ~compare:[%compare: _ * (int * _)]
        |> List.map ~f:(fun (name, (_, data)) ->
          [%sexp [ (name : string); (data : Nbt.t) ]])
      in
      [%sexp { data : Sexp.t list; handled = (t.handled : Set.M(String).t) }]
    ;;

    let of_tags (tags : Nbt.tag list) =
      let data =
        tags
        |> List.mapi ~f:(fun idx { name; data } -> name, (idx, data))
        |> Map.of_alist_exn (module String)
      in
      { data; handled = Set.empty (module String) }
    ;;

    let to_remaining t =
      let tags =
        Map.filter_keys t.data ~f:(fun name -> not (Set.mem t.handled name))
        |> Map.to_alist
        |> List.sort ~compare:[%compare: _ * (int * _)]
        |> List.map ~f:(fun (name, (_, data)) : Nbt.tag -> { name; data })
      in
      tags, { t with handled = Map.key_set t.data }
    ;;

    let get_opt t name =
      let%map.Option _, value = Map.find t.data name in
      value, { t with handled = Set.add t.handled name }
    ;;

    let get t name =
      match get_opt t name with
      | Some (data, t') -> data, t'
      | None -> raise_s [%message "Tags not found in compound nbt" ~nbt:(t : t)]
    ;;

    let ensure_all_handled t =
      match to_remaining t with
      | [], _ -> ()
      | remaining_unhandled, _ ->
        raise_s
          [%message
            "Not all tags handled in compound schema"
              (remaining_unhandled : Nbt.tag list)
              ~nbt:(t : t)]
    ;;
  end

  module Schema = struct
    type _ elem =
      | Required : string * ('a t[@sexp.opaque]) -> 'a elem
      | Optional : string * ('a t[@sexp.opaque]) -> 'a option elem
      | Rest : Nbt.tag list elem
    [@@deriving sexp_of]

    type _ t =
      | [] : unit t
      | ( :: ) : 'a elem * 'b t -> ('a -> 'b) t

    let parse_elem (type a) (elem : a elem) state : a * _ =
      match elem with
      | Required (name, conv) ->
        let data, state' = Parse_state.get state name in
        parse conv data, state'
      | Optional (name, conv) ->
        (match Parse_state.get_opt state name with
         | None -> None, state
         | Some (data, state') -> Some (parse conv data), state')
      | Rest -> Parse_state.to_remaining state
    ;;

    let parse_tags t tags =
      let rec aux : type a. a t -> Parse_state.t -> a Hlist.t =
        fun t state ->
        match t with
        | [] ->
          Parse_state.ensure_all_handled state;
          []
        | elem :: rest ->
          let x, state' = parse_elem elem state in
          x :: aux rest state'
      in
      aux t (Parse_state.of_tags tags)
    ;;

    let rec dump_tags : type a. a t -> a Hlist.t -> Nbt.tag list =
      fun t data ->
      match t, data with
      | Optional _ :: rest, None :: rest_data -> dump_tags rest rest_data
      | Optional (name, conv) :: rest, Some x :: rest_data ->
        { name; data = dump conv x } :: dump_tags rest rest_data
      | Required (name, conv) :: rest, x :: rest_data ->
        { name; data = dump conv x } :: dump_tags rest rest_data
      | Rest :: _, tags :: _ -> tags
      | [], [] -> []
    ;;
  end
end

let ( -- ) name t : _ Compound.Schema.elem = Required (name, t)
let ( --? ) name t : _ Compound.Schema.elem = Optional (name, t)
let compound = primitive Compound

let compound_schema schema =
  compound >>| Compound.Schema.parse_tags schema <<| Compound.Schema.dump_tags schema
;;

let compound_singleton name t =
  compound_schema [ name -- t ] >>| (fun [ x ] -> x) <<| fun x -> [ x ]
;;

let root t = compound_singleton "" t

let compound_map base =
  compound
  >>| (fun tags ->
        List.map tags ~f:(fun { name; data } -> name, parse base data)
        |> String.Map.of_alist_exn)
  <<| fun map ->
  Map.to_alist map
  |> List.map ~f:(fun (name, data) : Nbt.tag -> { name; data = dump base data })
;;

module type S = sig
  type t

  val nbt_conv : (t, t) t2
end

module Of_fields (M : sig
    module Typed_field : Typed_fields_lib.S

    val schema : 'a Typed_field.t -> 'a Compound.Schema.elem
  end) : S with type t := M.Typed_field.derived_on = struct
  module Key = struct
    type 'a t = 'a M.Typed_field.t

    let sexp_of_t _ t = [%sexp_of: M.Typed_field.Packed.t] { f = T t }

    include M.Typed_field.Type_ids
  end

  module Data_map =
    Univ_map.Make
      (Key)
      (struct
        type 'a t = 'a [@@deriving sexp_of]
      end)

  module Schema_map =
    Univ_map.Make
      (Key)
      (struct
        type 'a t = 'a Compound.Schema.elem [@@deriving sexp_of]
      end)

  let schema_map =
    lazy
      (Schema_map.of_alist_exn
         (List.map M.Typed_field.Packed.all ~f:(fun { f = T field } ->
            Schema_map.Packed.T (field, M.schema field))))
  ;;

  let get_schema field = Schema_map.find_exn (Lazy.force schema_map) field

  module Field_order = struct
    type _ t =
      | [] : unit t
      | ( :: ) : 'a M.Typed_field.t * 'b t -> ('a -> 'b) t

    let to_hlist t data =
      let rec aux : type a. a t -> a Hlist.t = function
        | [] -> []
        | field :: rest -> M.Typed_field.get field data :: aux rest
      in
      aux t
    ;;

    let rec to_data_map_elems : type a. a t -> a Hlist.t -> Data_map.Packed.t list =
      fun t hlist ->
      match t, hlist with
      | [], [] -> []
      | field :: fields, data :: rest -> T (field, data) :: to_data_map_elems fields rest
    ;;
  end

  module Mapped_schema = struct
    type t =
      | T :
          { schema : 'a Compound.Schema.t
          ; fields : 'a Field_order.t
          }
          -> t

    let t =
      lazy
        (let field_order =
           let normal, rest =
             List.partition_tf M.Typed_field.Packed.all ~f:(fun { f = T field } ->
               match get_schema field with
               | Required _ | Optional _ -> true
               | Rest -> false)
           in
           normal @ rest
         in
         List.fold_right
           field_order
           ~f:(fun packed (T { schema; fields }) ->
             let (T field) = packed.f in
             T { schema = get_schema field :: schema; fields = field :: fields })
           ~init:(T { schema = []; fields = [] }))
    ;;
  end

  let nbt_conv =
    Lazy
      (let%map.Lazy (T { schema; fields }) = Mapped_schema.t in
       compound_schema schema
       <<| (fun x -> Field_order.to_hlist fields x)
       >>| fun hlist ->
       let data_map =
         Field_order.to_data_map_elems fields hlist |> Data_map.of_alist_exn
       in
       M.Typed_field.create
         { f =
             (fun field ->
               let res = Data_map.find_exn data_map (M.Typed_field.globalize0 field) in
               res)
         })
  ;;
end

module Of_stringable (M : Stringable) = struct
  let nbt_conv = stringable (module M)
end
