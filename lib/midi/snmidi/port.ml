open! Core
open Jsonaf.Export

module Id = struct
  include
    (val String_id.make
           ~caller_identity:String_id.legacy_identity
           ~module_name:"Snmidi.Port.Id"
           ~include_default_validation:true
           ())

  include functor Jsonaf.Jsonafable.Of_stringable
end

type t =
  { id : Id.t
  ; name : string
  }
[@@deriving jsonaf, sexp_of]
