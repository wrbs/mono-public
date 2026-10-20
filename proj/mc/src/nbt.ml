open! Core

type _ type_ =
  | Byte : Int_repr.Int8.t type_
  | Short : Int_repr.Int16.t type_
  | Int : Int_repr.Int32.t type_
  | Long : Int_repr.Int64.t type_
  | Float : Int_repr.Uint32.t type_
  | Double : Int_repr.Uint64.t type_
  | Byte_array : string type_
  | String : String.Utf8.t type_
  | List : nbt_list type_
  | Compound : tag list type_
  | Int_array : Int_repr.Int32.t iarray type_
  | Long_array : Int_repr.Int64.t iarray type_

and nbt_list =
  | Values : ('payload type_ * 'payload iarray) -> nbt_list
  | Empty_using_end_tag : nbt_list

and t = T : 'a type_ * 'a -> t

and tag =
  { name : string
  ; data : t
  }
[@@deriving sexp_of]

let rec sexp_of_t t =
  let add_suffix suffix : Sexp.t -> Sexp.t = function
    | Atom s -> Atom (s ^ suffix)
    | List _ -> failwith "expected atom"
  in
  let (T (type_, data)) = t in
  let array contents ~prefix ~type_ =
    let inner = Iarray.map contents ~f:(fun payload -> T (type_, payload)) in
    [%sexp [ (prefix : string); (inner : t iarray) ]]
  in
  match type_ with
  | Byte -> Int_repr.Int8.sexp_of_t data |> add_suffix "b"
  | Short -> Int_repr.Int16.sexp_of_t data |> add_suffix "s"
  | Int -> Int_repr.Int32.sexp_of_t data |> add_suffix "d"
  | Long -> Int_repr.Int64.sexp_of_t data |> add_suffix "l"
  | Float -> Int_repr.Uint32.sexp_of_t data |> add_suffix ":float"
  | Double -> Int_repr.Uint64.sexp_of_t data |> add_suffix ":double"
  | String -> String.Utf8.sexp_of_t data
  | Byte_array ->
    let i8s =
      String.to_list data
      |> List.map ~f:(fun c ->
        Char.to_int c |> Int_repr.Uint8.of_base_int_exn |> Int_repr.Int8.of_uint8_wrap)
      |> Iarray.of_list
    in
    array i8s ~prefix:"byte-array" ~type_:Byte
  | List ->
    (match data with
     | Empty_using_end_tag -> [%sexp []]
     | Values (type_, elems) -> array elems ~prefix:"list" ~type_)
  | Compound -> [%sexp (data : tag list)]
  | Int_array -> array data ~prefix:"int-array" ~type_:Int
  | Long_array -> array data ~prefix:"long-array" ~type_:Long

and sexp_of_tag { name; data } = [%sexp [ (name : string); (data : t) ]]

let sexp_of_nbt_list x = sexp_of_t (T (List, x))

type wire_id =
  | End : wire_id
  | Type : _ type_ -> wire_id

let read_id ~buf =
  match Iobuf.Consume.uint8 buf with
  | 0x00 -> End
  | 0x01 -> Type Byte
  | 0x02 -> Type Short
  | 0x03 -> Type Int
  | 0x04 -> Type Long
  | 0x05 -> Type Float
  | 0x06 -> Type Double
  | 0x07 -> Type Byte_array
  | 0x08 -> Type String
  | 0x09 -> Type List
  | 0x0A -> Type Compound
  | 0x0B -> Type Int_array
  | 0x0C -> Type Long_array
  | id -> raise_s [%message "Unknown tag type id" (id : Int.Hex.t)]
;;

let rec read_compound ~buf =
  match read_id ~buf with
  | End -> []
  | Type type_ ->
    let tag = read_named_tag type_ ~buf in
    tag :: read_compound ~buf

and read_named_tag : type a. a type_ -> buf:_ -> tag =
  fun type_ ~buf ->
  let name_length = Iobuf.Consume.uint16_be buf in
  let name = Iobuf.Consume.stringo buf ~len:name_length in
  let payload = read_payload type_ ~buf in
  { name; data = T (type_, payload) }

and read_payload : type a. a type_ -> buf:_ -> a =
  fun type_ ~buf ->
  match type_ with
  | Byte -> Iobuf.Consume.Int_repr.int8 buf
  | Short -> Iobuf.Consume.Int_repr.int16_be buf
  | Int -> Iobuf.Consume.Int_repr.int32_be buf
  | Long -> Iobuf.Consume.Int_repr.int64_be buf
  | Float -> Iobuf.Consume.Int_repr.uint32_be buf
  | Double -> Iobuf.Consume.Int_repr.uint64_be buf
  | Byte_array ->
    read_array Byte ~buf
    |> Iarray.to_list
    |> List.map ~f:(fun c -> Int_repr.Int8.to_base_int c |> Char.of_int_exn)
    |> String.of_list
  | String ->
    let len = Iobuf.Consume.uint16_be buf in
    Iobuf.Consume.stringo buf ~len |> String.Utf8.of_string
  | List ->
    (match read_id ~buf with
     | Type sub_id -> Values (sub_id, read_array sub_id ~buf)
     | End ->
       let length = Iobuf.Consume.int32_be buf in
       if length <> 0 then raise_s [%message "Expected 0 length list as type is 0x00"];
       Empty_using_end_tag)
  | Compound -> read_compound ~buf
  | Int_array -> read_array Int ~buf
  | Long_array -> read_array Long ~buf

and read_array : type a. a type_ -> buf:_ -> a iarray =
  fun type_ ~buf ->
  let size = Iobuf.Consume.int32_be buf in
  if size < 0 then raise_s [%message "Size must be positive" (size : int)];
  Iarray.init size ~f:(fun _ -> read_payload type_ ~buf)
;;

let parse_uncompressed data =
  let buf = Iobuf.of_string data in
  let t =
    match read_id ~buf with
    | End -> raise_s [%message "Unexpected END for root data"]
    | Type type_ -> read_named_tag type_ ~buf
  in
  if not (Iobuf.is_empty buf)
  then
    raise_s
      [%message
        "Got unprocessed data" ~remaining:(Iobuf.Hexdump.to_string_hum buf : string)];
  t
;;

let inflate input =
  let o = Bigstring.create Gz.io_buffer_size in
  let buffer = Buffer.create 1024 in
  let decoder = Gz.Inf.decoder (`String input) ~o in
  let flush decoder =
    Bigstring.to_string o ~len:(Gz.io_buffer_size - Gz.Inf.dst_rem decoder)
    |> Buffer.add_string buffer;
    Gz.Inf.flush decoder
  in
  let rec aux decoder =
    match Gz.Inf.decode decoder with
    | `Malformed error -> Or_error.error_s [%message "Malformed input" (error : string)]
    | `Await _ -> raise_s [%message "should be impossible"]
    | `Flush decoder -> aux (flush decoder)
    | `End decoder ->
      let _ = flush decoder in
      Ok (Buffer.contents buffer)
  in
  aux decoder
;;

let parse data =
  match inflate data with
  | Ok inflated -> parse_uncompressed inflated
  | Error _ -> parse_uncompressed data
;;

let is_same_type (type a b) (a : a type_) (b : b type_) : (a, b) Type_equal.t option =
  match a, b with
  | Byte, Byte -> Some T
  | Short, Short -> Some T
  | Int, Int -> Some T
  | Long, Long -> Some T
  | Float, Float -> Some T
  | Double, Double -> Some T
  | Byte_array, Byte_array -> Some T
  | String, String -> Some T
  | List, List -> Some T
  | Compound, Compound -> Some T
  | Int_array, Int_array -> Some T
  | Long_array, Long_array -> Some T
  | ( ( Byte
      | Short
      | Int
      | Long
      | Float
      | Double
      | Byte_array
      | String
      | List
      | Compound
      | Int_array
      | Long_array )
    , _ ) -> None
;;
