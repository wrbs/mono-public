open! Core
open Jsonaf.Export

module Id = struct
  include
    String_id.Make
      (struct
        let module_name = "Snmidi.Port.Id"
      end)
      ()

  include functor Jsonaf.Jsonafable.Of_stringable
end

type t =
  { id : Id.t
  ; name : string
  }
[@@deriving jsonaf, sexp_of]
