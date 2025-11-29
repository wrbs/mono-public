open! Core

module T = struct
  type t = V2 [@rename "2.0"] [@@deriving string]
end

include T
include Jsonaf.Jsonafable.Of_stringable (T)
