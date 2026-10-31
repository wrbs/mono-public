module Yojson_conv = Ppx_yojson_conv_lib.Yojson_conv
include Ppx_yojson_conv_lib.Yojsonable

module Of_yojsonable = struct
  module V1
      (Yojsonable : S)
      (M : sig
         type t

         val of_yojsonable : Yojsonable.t -> t
         val to_yojsonable : t -> Yojsonable.t
       end) =
  struct
    let t_of_yojson yojson = M.of_yojsonable (Yojsonable.t_of_yojson yojson)
    let yojson_of_t mt = M.to_yojsonable mt |> Yojsonable.yojson_of_t
  end
end

module Of_stringable = struct
  module V1 (M : Base.Stringable.S) : S with type t := M.t = struct
    let t_of_yojson = function
      | `String s as json ->
        (match M.of_string s with
         | x -> x
         | exception exn -> Yojson_conv.of_yojson_error_exn exn json)
      | json -> Yojson_conv.of_yojson_error "string expected" json
    ;;

    let yojson_of_t (t : M.t) : Yojson.Safe.t = `String (M.to_string t)
  end
end
