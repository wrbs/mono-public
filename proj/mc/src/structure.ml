open! Core

module Block_id = struct
  include
    String_id.Make
      (struct
        let module_name = "Block_id"
      end)
      ()

  include functor Nbt_conv.Of_stringable
end

module Pos = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving compare]

  let sexp_of_t { x; y; z } = [%sexp [%string "%{x#Int} %{y#Int} %{z#Int}"]]

  let nbt_conv =
    let open Nbt_conv in
    list i32
    <<| (fun { x; y; z } -> [ x; y; z ])
    >>| function
    | [ x; y; z ] -> { x; y; z }
    | values -> raise_s [%message "Pos.nbt_conv: expected 3 values" (values : int list)]
  ;;

  include functor Comparable.Make_plain
end

module Repr = struct
  module Block = struct
    type t =
      { state : int
      ; pos : Pos.t
      ; nbt : Nbt.tag list option
      }
    [@@deriving sexp_of, typed_fields]

    let schema : type a. a Typed_field.t -> a Nbt_conv.Compound.Schema.elem =
      let open Nbt_conv in
      function
      | State -> "state" -- i32
      | Pos -> "pos" -- Pos.nbt_conv
      | Nbt -> "nbt" --? compound
    ;;

    include functor Nbt_conv.Of_fields
  end

  module Block_state = struct
    type t =
      { name : Block_id.t
      ; properties : string String.Map.t
      }
    [@@deriving sexp_of, typed_fields]

    let schema : type a. a Typed_field.t -> a Nbt_conv.Compound.Schema.elem =
      let open Nbt_conv in
      function
      | Name -> "Name" -- Block_id.nbt_conv
      | Properties -> "Properties" -- compound_map string
    ;;

    include functor Nbt_conv.Of_fields
  end

  let empty_list =
    let open Nbt_conv in
    primitive List
    >>| (function
           | Empty_using_end_tag -> ()
           | Values (_, _) -> ())
    <<| fun () -> Empty_using_end_tag
  ;;

  type t =
    { size : Pos.t
    ; blocks : Block.t list
    ; palette : Block_state.t list
    ; entities : unit
    ; data_version : int
    }
  [@@deriving sexp_of, typed_fields]

  let schema : type a. a Typed_field.t -> a Nbt_conv.Compound.Schema.elem =
    let open Nbt_conv in
    function
    | Size -> "size" -- Pos.nbt_conv
    | Blocks -> "blocks" -- list Block.nbt_conv
    | Palette -> "palette" -- list Block_state.nbt_conv
    | Entities -> "entities" -- empty_list
    | Data_version -> "DataVersion" -- i32
  ;;

  include functor Nbt_conv.Of_fields
end
