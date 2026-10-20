open! Core
module Hlist = Heterogeneous_list

type ('a, 'b) t2
type 'a t = ('a, 'a) t2

module type S = sig
  type t

  val nbt_conv : (t, t) t2
end

val parse : ('a, 'b) t2 -> Nbt.t -> 'b
val dump : ('a, 'b) t2 -> 'a -> Nbt.t
val map : ('a, 'b) t2 -> f:('b -> 'c) -> ('a, 'c) t2
val unmap : ('a, 'b) t2 -> f:('c -> 'a) -> ('c, 'b) t2
val ( >>| ) : ('a, 'b) t2 -> ('b -> 'c) -> ('a, 'c) t2
val ( <<| ) : ('a, 'b) t2 -> ('c -> 'a) -> ('c, 'b) t2
val primitive : 'a Nbt.type_ -> 'a t
val i8' : Int_repr.Int8.t t
val i16' : Int_repr.Int16.t t
val i32' : Int_repr.Int32.t t
val i64' : Int_repr.Int64.t t
val i8 : int t
val i16 : int t
val i32 : int t
val i64 : int t
val list_as_iarray : ('a, 'b) t2 -> ('a iarray, 'b iarray) t2
val list : ('a, 'b) t2 -> ('a list, 'b list) t2
val utf8 : String.Utf8.t t
val string : string t
val stringable : (module Stringable.S with type t = 'a) -> 'a t

module Compound : sig
  module Schema : sig
    type _ elem =
      | Required : string * 'a t -> 'a elem
      | Optional : string * 'a t -> 'a option elem
      | Rest : Nbt.tag list elem

    type _ t =
      | [] : unit t
      | ( :: ) : 'a elem * 'b t -> ('a -> 'b) t
  end
end

val ( -- ) : string -> 'a t -> 'a Compound.Schema.elem
val ( --? ) : string -> 'a t -> 'a option Compound.Schema.elem
val compound : Nbt.tag list t
val compound_schema : 'a Compound.Schema.t -> 'a Hlist.t t
val compound_singleton : string -> 'a t -> 'a t
val compound_map : 'a t -> 'a String.Map.t t
val root : 'a t -> 'a t

module Of_fields (M : sig
    module Typed_field : Typed_fields_lib.S

    val schema : 'a Typed_field.t -> 'a Compound.Schema.elem
  end) : S with type t := M.Typed_field.derived_on

module Of_stringable (M : Stringable.S) : S with type t := M.t
