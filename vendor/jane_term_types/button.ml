(* Note that [assert]s are disabled in this library (see the jbuild) *)
open Core

module T = struct
  type t =
    [ `Button1
    | `Button2
    | `Button3
    | `Button1_double_clicked
    | `Button2_double_clicked
    | `Button3_double_clicked
    ]
  [@@deriving sexp, bin_io]
end

include T
include Sexpable.To_stringable (T)

type click_handler = button:t -> unit
