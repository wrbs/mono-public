open! Core

(** Translates the high-level [Fold] construct (repetition with bounds, separators, and
    accumulation) into the lower-level engine grammar. *)

module How_to_fold : sig
  type ('acc, 'item) t =
    | Unit : (unit, unit) t
    | Fold :
        { init : 'acc
        ; f : 'acc -> 'item -> 'acc
        }
        -> ('acc, 'item) t
end

module Spec : sig
  type ('acc, 'item) t =
    { how_to_fold : ('acc, 'item) How_to_fold.t
    ; bounds : Bound.t * Bound.t
    ; item : 'item Defs.Grammar.t
    ; sep : unit Defs.Grammar.t option
    }
end

val build_grammar : ('acc, _) Spec.t -> 'acc Defs.Grammar.t
