open! Import

module Global : sig
  type t =
    [ `Window of Types.Globals.window Js.t
    | `Dedicated_worker of Types.Globals.dedicated_worker_global_scope Js.t
    | `Shared_worker of Types.Globals.shared_worker_global_scope Js.t
    | `Unknown of < > Js.t
    | `Undefined
    ]

  (** [get] returns the current JavaScript global scope as a variant type, identifying
      which kind of global scope is available in the current context.

      [`Unknown] is the fallback variant for unrecognized global scopes *)
  val get : unit -> t

  (** [is_in_context] retrieves the global object and checks to see whether or not we are
      in the provided [context] *)
  val is_in_context : 'a. context:'a Context.t -> t -> bool

  (** [window] returns [Window] if it is the global scope. Otherwise, returns [None] *)
  val window : unit -> Types.Globals.window Js.t option

  (** [window] returns [Window] if it is the global scope. Otherwise, raises *)
  val window_exn : unit -> Types.Globals.window Js.t

  (** [dedicated_worker] returns [DedicatedWorkerGlobalScope] if it is the global scope.
      Otherwise, returns [None] *)
  val dedicated_worker : unit -> Types.Globals.dedicated_worker_global_scope Js.t option

  (** [dedicated_worker] returns [DedicatedWorkerGlobalScope] if it is the global scope.
      Otherwise, raises *)
  val dedicated_worker_exn : unit -> Types.Globals.dedicated_worker_global_scope Js.t

  (** [shared_worker] returns [SharedWorkerGlobalScope] if it is the global scope.
      Otherwise, returns [None] *)
  val shared_worker : unit -> Types.Globals.shared_worker_global_scope Js.t option

  (** [shared_worker] returns [SharedWorkerGlobalScope] if it is the global scope.
      Otherwise, raises *)
  val shared_worker_exn : unit -> Types.Globals.shared_worker_global_scope Js.t
end
