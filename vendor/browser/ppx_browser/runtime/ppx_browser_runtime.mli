(** [ppx_browser_runtime] currently just re-exports things from the restricted library
    [browser_manually_written_bindings] so that the ppx can access them *)

module Unsafe = Browser_manually_written_bindings.Unsafe

(* We should solely export the [check_context] method here so that there is no way outside
   of the restricted libraries to create this context checker *)
module Unsafe_context_checker : sig
  val check_context
    :  context:'a Browser_manually_written_bindings.Context.t
    -> Browser_manually_written_bindings.Unsafe_context_checker.t
    -> bool
end
