open! Core
open Jsonaf.Export

module Success = struct
  module Json = struct
    type t =
      { jsonrpc : Version.t
      ; result : Jsonaf.t
      ; id : Id.t
      }
    [@@deriving jsonaf]
  end

  module T = struct
    type t =
      { result : Jsonaf.t
      ; id : Id.t
      }
    [@@deriving sexp_of]

    let of_jsonafable : Json.t -> t = function
      | { jsonrpc = _; result; id } -> { result; id }
    ;;

    let to_jsonafable : t -> Json.t = function
      | { result; id } -> { jsonrpc = V2; result; id }
    ;;
  end

  include T
  include Jsonaf.Jsonafable.Of_jsonafable (Json) (T)
end

module Failure = struct
  module Json = struct
    type t =
      { jsonrpc : Version.t
      ; error : Rpc_error.t
      ; id : Id.t
      }
    [@@deriving jsonaf]
  end

  module T = struct
    type t =
      { error : Rpc_error.t
      ; id : Id.t
      }
    [@@deriving sexp_of]

    let of_jsonafable : Json.t -> t = function
      | { jsonrpc = _; error; id } -> { error; id }
    ;;

    let to_jsonafable : t -> Json.t = function
      | { error; id } -> { jsonrpc = V2; error; id }
    ;;
  end

  include T
  include Jsonaf.Jsonafable.Of_jsonafable (Json) (T)
end

module Output = struct
  type t =
    | Success of Success.t
    | Failure of Failure.t
  [@@deriving sexp_of]

  let jsonaf_of_t = function
    | Success success -> [%jsonaf_of: Success.t] success
    | Failure failure -> [%jsonaf_of: Failure.t] failure
  ;;

  let t_of_jsonaf jsonaf =
    match [%of_jsonaf: Failure.t] jsonaf with
    | failure -> Failure failure
    | exception _ -> Success ([%of_jsonaf: Success.t] jsonaf)
  ;;
end

type t =
  | Single of Output.t
  | Batch of Output.t list
[@@deriving sexp_of]

let jsonaf_of_t = function
  | Single output -> [%jsonaf_of: Output.t] output
  | Batch outputs -> [%jsonaf_of: Output.t list] outputs
;;

let t_of_jsonaf (json : Jsonaf.t) =
  match json with
  | `Array _ -> Batch ([%of_jsonaf: Output.t list] json)
  | _ -> Single ([%of_jsonaf: Output.t] json)
;;

let of_string s = Jsonaf.of_string s |> [%of_jsonaf: t]
let of_string_opt s = Option.try_with (fun () -> Jsonaf.of_string s |> [%of_jsonaf: t])
let to_string t = t |> [%jsonaf_of: t] |> Jsonaf.to_string
let to_string_hum t = t |> [%jsonaf_of: t] |> Jsonaf.to_string_hum
