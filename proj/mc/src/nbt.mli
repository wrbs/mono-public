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

val parse : string -> tag
val is_same_type : 'a type_ -> 'b type_ -> ('a, 'b) Type_equal.t option
