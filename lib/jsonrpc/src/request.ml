open! Core
open Jsonaf.Export

module Request_format = struct
  module Json = struct
    type t =
      { jsonrpc : Version.t
      ; method_ : string [@jsonaf.key "method"]
      ; params : Jsonaf.t option [@jsonaf.option]
      ; id : Id.t option [@jsonaf.option]
      }
    [@@deriving jsonaf]
  end

  module T = struct
    type t =
      { method_ : string
      ; params : Jsonaf.t option [@sexp.option]
      ; id : Id.t option [@sexp.option]
      }
    [@@deriving sexp_of]

    let of_jsonafable : Json.t -> t = function
      | { jsonrpc = _; method_; params; id } -> { method_; params; id }
    ;;

    let to_jsonafable : t -> Json.t = function
      | { method_; params; id } -> { jsonrpc = V2; method_; params; id }
    ;;
  end

  include T
  include Jsonaf.Jsonafable.Of_jsonafable (Json) (T)
end

module Method_call = struct
  type t =
    { method_ : string
    ; params : Jsonaf.t option [@sexp.option]
    ; id : Id.t
    }
  [@@deriving sexp_of]
end

module Notification = struct
  type t =
    { method_ : string
    ; params : Jsonaf.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Invalid = struct
  type t = { id : Id.t option [@jsonaf.option] }
  [@@jsonaf.allow_extra_fields] [@@deriving sexp_of, jsonaf]

  (* Anything invalid is 'valid' with id = None *)
  let t_of_jsonaf jsonaf =
    try t_of_jsonaf jsonaf with
    | _ -> { id = None }
  ;;
end

module Call = struct
  type t =
    | Method_call of Method_call.t
    | Notification of Notification.t
    | Invalid of Invalid.t
  [@@deriving sexp_of]

  include
    Jsonaf.Jsonafable.Of_jsonafable
      (Request_format)
      (struct
        type nonrec t = t

        let of_jsonafable (request : Request_format.t) =
          let%tydi { method_; params; _ } = request in
          match request.id with
          | None -> Notification { method_; params }
          | Some id -> Method_call { method_; params; id }
        ;;

        let to_jsonafable t : Request_format.t =
          match t with
          | Method_call { method_; params; id } -> { method_; params; id = Some id }
          | Notification { method_; params } -> { method_; params; id = None }
          | Invalid _ -> raise_s [%message "unreachable: handled in wrapper"]
        ;;
      end)

  (* Wrap for invalid *)
  let t_of_jsonaf jsonaf =
    try t_of_jsonaf jsonaf with
    | _ -> Invalid ([%of_jsonaf: Invalid.t] jsonaf)
  ;;

  let jsonaf_of_t = function
    | Invalid invalid -> [%jsonaf_of: Invalid.t] invalid
    | t -> jsonaf_of_t t
  ;;
end

type t =
  | Single of Call.t
  | Batch of Call.t list
[@@deriving sexp_of]

let jsonaf_of_t = function
  | Single call -> [%jsonaf_of: Call.t] call
  | Batch calls -> [%jsonaf_of: Call.t list] calls
;;

let t_of_jsonaf (json : Jsonaf.t) =
  match json with
  | `Array [] -> Single (Invalid { id = None })
  | `Array _ -> Batch ([%of_jsonaf: Call.t list] json)
  | _ -> Single ([%of_jsonaf: Call.t] json)
;;

let of_string s = Jsonaf.of_string s |> [%of_jsonaf: t]
let of_string_opt s = Option.try_with (fun () -> Jsonaf.of_string s |> [%of_jsonaf: t])
let to_string t = t |> [%jsonaf_of: t] |> Jsonaf.to_string
let to_string_hum t = t |> [%jsonaf_of: t] |> Jsonaf.to_string_hum
