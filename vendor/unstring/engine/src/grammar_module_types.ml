open! Core
include Grammar_module_types_intf
include Definitions

module Make_derived (M : sig
    type terminal [@@deriving sexp_of]
    type input [@@deriving sexp_of]
  end) =
struct
  open M

  type 'a t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Terminal : terminal -> unit t
    | Capture : unit t * (('a, input) Definitions.Capture.t[@sexp.opaque]) -> 'a t
    | Capture_also :
        'a t * (('a -> 'b, input) Definitions.Capture.t[@sexp.opaque])
        -> 'b t
    | Union : 'a t list -> 'a t
    | Append : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t
    | Concat_unit : unit t list -> unit t
    | Star : 'a t * (('acc, 'a) Definitions.Fold.t[@sexp.opaque]) -> 'acc t
    | Star_unit : unit t -> unit t
  [@@deriving sexp_of]
end
