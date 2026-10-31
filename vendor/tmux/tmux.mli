open! Core
open Async

(** Represents a tmux session running some command *)
type t

module Size : sig
  type t =
    { width : int
    ; height : int
    }

  val default : t

  (* width = 80, height = 30 *)
end

module Cursor : sig
  (** [Cursor.t] represents the program's cursor.

      The top-left corner is [{x = 0 ; y = 0 }]. [y] increases downwards and [x] increases
      right-wards.

      [cursor_character] is the character underneath the tmux cursor. It is a [string]
      instead of a [char] to support multi-char unicode characters. *)
  type t =
    { x : int
    ; y : int
    ; cursor_character : string
    }
  [@@deriving sexp_of]
end

(** By default, all creators wait for [dump_screen] to return at least one string
    containing something other than whitespace (a non-empty ui). By providing
    [don't_wait_for_ui], no screen dumping or waiting for the application will occur. *)
type 'a creator :=
  ?size:Size.t
  -> ?don't_wait_for_ui:unit
  -> ?support_hyperlinks:bool (** default: true *)
  -> ?extra_args:string list (** Extra arguments to pass to tmux, e.g. ["-u"] for UTF-8 *)
  -> ?working_dir:string
  -> ?env:Core_unix.env
  -> ?mouse:unit (** Enable mouse support in the tmux session via [set -g mouse on]. *)
  -> command:string
  -> 'a

(** Start a new isolated tmux, and run the string in bash *)
val create : (unit -> t Deferred.Or_error.t) creator

(** A [Key.t] extends [Jane_term_types.Key.t] with [`Raw_escape] text and bracketed
    [`Paste] events. *)
module Key : sig
  type t =
    [ Jane_term_types.Key.t
    | `Raw_escape of string
    | `Paste of [ `Start | `End ]
    ]
end

(** Send [Key.t]s to the specified tmux. *)
val send_keys : t -> Key.t list -> unit Deferred.Or_error.t

(** [send_key t key] sends the keystroke [key] to [t]. *)
val send_key : t -> Key.t -> unit Deferred.Or_error.t

(** Send the chars in the string to the specified tmux to be interpreted as text rather
    than control characters *)
val send_chars : t -> string -> unit Deferred.Or_error.t

(** [send_keys_raw] lets you raw strings as keys, letting you send unicode characters or
    bracketed paste events to tmux. *)
val send_keys_raw : t -> string list -> unit Deferred.Or_error.t

(** [scroll] emulates tmux wheel scrolling for detached sessions managed through the tmux
    control plane, matching tmux's default wheel behavior. [`Up] enters copy mode if
    needed, then scrolls up. [`Down] scrolls down when already in copy mode. *)
val scroll : t -> [ `Up | `Down ] -> unit Deferred.Or_error.t

(** Resize the tmux window *)
val resize : t -> Size.t -> unit Deferred.Or_error.t

(** Immediately dumps the screen as a list of lines, optionally with escape sequences. *)
val dump_screen
  :  ?preserve_trailing_spaces:unit
  -> ?dump_escape_sequences:unit
  -> t
  -> string list Deferred.Or_error.t

(** Like [dump_screen], but returns the viewport currently visible to the user. This
    accounts for tmux copy-mode scroll position, which plain [capture-pane] does not. *)
val dump_visible_screen
  :  ?preserve_trailing_spaces:unit
  -> ?dump_escape_sequences:unit
  -> height:int
  -> t
  -> string list Deferred.Or_error.t

val get_cursor : t -> Cursor.t Deferred.Or_error.t

(** Returns the PID of the process running in the tmux pane. *)
val pane_pid : t -> Pid.t Deferred.Or_error.t

(** Kills the tmux session and all non-daemonized processes inside of it. This operation
    is idempotent. *)
val close : t -> unit Deferred.Or_error.t

(** [closed t] becomes determined when [t] is closed. *)
val closed : t -> unit Deferred.t

(** Runs [command] in a [t] and provides it to [f]. Calls [close] when [f] is determined *)
val with_command : (f:(t -> 'a Deferred.t) -> unit -> 'a Deferred.Or_error.t) creator

(** A serializable identifier for a tmux session, containing enough information to
    reconnect to it from another process. *)
module Session_id : sig
  type t [@@deriving sexp]
end

val session_id : t -> Session_id.t

(** Attach to an existing tmux session identified by a [Session_id.t]. This is intended
    for use when one process creates a tmux session and another process needs to interact
    with it (e.g., sending keys, dumping the screen).

    The caller does not take ownership of the session lifecycle -- the original creator is
    responsible for managing the session. Calling [close] on an attached session will only
    shut down the attached client for [t] but will not kill the session. Conversely, if
    the underlying session is killed, the attached client will be automatically closed. *)
val attach : Session_id.t -> t Deferred.Or_error.t

module For_testing : sig
  module Name : sig
    type t

    val create : unit -> t
    val to_string : t -> string
  end

  val create : (name:Name.t -> unit -> t Deferred.Or_error.t) creator
  val save_buffer_path : t -> string
  val socket_path : t -> string
  val name : t -> Name.t
  val tmux_executable_path : support_hyperlinks:bool -> string Deferred.t
end
