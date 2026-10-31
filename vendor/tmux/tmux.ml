open! Core
open Async
open Import

module Key = struct
  type t =
    [ Jane_term_types.Key.t
    | `Raw_escape of string
    | `Paste of [ `Start | `End ]
    ]
end

module Size = struct
  type t =
    { width : int
    ; height : int
    }

  let default = { width = 80; height = 30 }
end

module Cursor = struct
  type t =
    { x : int
    ; y : int
    ; cursor_character : string
    }
  [@@deriving sexp_of]
end

(** A [Name] is used as the "-S" argument to tmux, in order to partition tmux sessions via
    unix socket. This ensures that if the machine has running tmux sessions, or if there
    are multiple simultaneous tests, they will not interfere. From the man page:

    -S socket-path Specify a full alternative path to the server socket. If -S is
    specified, the default socket directory is not used and any -L flag is ignored. *)
module Name : sig
  type t = private string [@@deriving sexp]

  val create : rundir:string -> t
  val to_string : t -> string
end = struct
  type t = string [@@deriving sexp]

  let to_string = Fn.id

  module Counter = Unique_id.Int63 ()

  let create ~rundir =
    (* Use the calling process and an incrementing number to avoid collisions *)
    rundir ^/ sprintf !"ocaml-tmux.%{Pid}.%{Counter}" (Unix.getpid ()) (Counter.create ())
  ;;
end

module Close_result = struct
  type t =
    { screen : string list Or_error.t
    ; kill_session_result : unit Or_error.t
    }
  [@@deriving sexp_of]
end

type closed =
  { result : Close_result.t Or_error.t Deferred.t
  ; why_closed : Info.t Deferred.t
  }
[@@deriving sexp_of]

module Ownership = struct
  type t =
    | Created
    | Attached of { client : Process.t Deferred.t }
  [@@deriving sexp_of]
end

type t =
  { name : Name.t
  ; prog : string
  ; extra_args : string list
  ; closed : closed Ivar.t
  ; ownership : Ownership.t
  }
[@@deriving sexp_of]

module Session_id = struct
  (** [Session_id.t] is semi-stable and flexibly extended. Because sessions are ephemeral,
      it won't be persisted, but it should be possible to interpret with version skew. *)
  type t =
    { name : Name.t
    ; prog : string
    ; extra_args : string list
    }
  [@@sexp.allow_extra_fields] [@@deriving sexp]
end

let session_id t = { Session_id.name = t.name; prog = t.prog; extra_args = t.extra_args }
let socket_path t = (t.name :> string)
let session_end_output_path t = (t.name :> string) ^ ".session-end-output"

let last_screen t =
  Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
    Reader.file_lines (session_end_output_path t))
;;

let gen_args t ~command = t.extra_args @ [ "-S"; socket_path t ] @ command
let prog ~support_hyperlinks:_ = Deferred.return "tmux"
let looping_pause_time = sec 0.1

let wait_for_socket_existence t ~timeout =
  let has_timed_out = Clock.after timeout in
  Deferred.repeat_until_finished () (fun () ->
    match Ivar.peek t.closed with
    | Some { why_closed; _ } ->
      let%map why_closed in
      `Finished
        (Or_error.error
           "tmux session closed while waiting for socket"
           why_closed
           [%sexp_of: Info.t])
    | None ->
      (match Deferred.peek has_timed_out with
       | Some () ->
         return
           (`Finished (Or_error.error_s [%message "Timed out" (timeout : Time.Span.t)]))
       | None ->
         (match%bind Unix.access (socket_path t) [ `Read ] with
          | Ok () -> return (`Finished (Ok ()))
          | Error _ ->
            let%map () = Clock.after looping_pause_time in
            `Repeat ())))
;;

let run_aux t ~command =
  let tag_error = function
    | Ok _ as ok -> return ok
    | Error _ as e ->
      let%map last_screen = last_screen t in
      let tag =
        Sexp.to_string [%message "" (last_screen : string list Or_error.t) (t : t)]
      in
      Or_error.tag e ~tag
  in
  Deferred.bind
    (Process.run_lines ~prog:t.prog ~args:(gen_args t ~command) ())
    ~f:tag_error
;;

let run_no_output_aux t ~command =
  let open Deferred.Or_error.Let_syntax in
  let%bind output = run_aux t ~command in
  if List.is_empty output
  then return ()
  else
    Deferred.Or_error.error_s
      [%message
        "BUG: unexpected output from tmux command"
          (command : string list)
          (output : string list)]
;;

let is_closed t = Ivar.is_full t.closed

let close t =
  match Ivar.peek t.closed with
  | Some { result; _ } -> result
  | None ->
    let result =
      match t.ownership with
      | Created ->
        (* Try to get the debug capture output *)
        let%bind screen = last_screen t in
        let%bind result =
          Deferred.Or_error.try_with_join ~run:`Schedule ~rest:`Log (fun () ->
            run_no_output_aux t ~command:[ "kill-session" ])
        in
        let%bind () = Unix.unlink (socket_path t) in
        let%bind () =
          (* This may not exist, so do not fail if we cannot unlink *)
          Monitor.try_with ~run:`Schedule ~rest:`Log (fun () ->
            Unix.unlink (session_end_output_path t))
          >>| (ignore : (unit, exn) Result.t -> unit)
        in
        Deferred.Or_error.return { Close_result.screen; kill_session_result = result }
      | Attached { client } ->
        (* Signal the client process to kill and detach. Don't kill the session or clean
           up files. *)
        Deferred.upon client (fun client -> Process.send_signal client Signal.term);
        Deferred.Or_error.return
          { Close_result.screen = Ok []; kill_session_result = Ok () }
    in
    let ownership = t.ownership in
    let why_closed =
      let%map result in
      Info.create_s
        [%message
          "[close] was invoked"
            (ownership : Ownership.t)
            (result : Close_result.t Or_error.t)]
    in
    Ivar.fill_if_empty t.closed { result; why_closed };
    result
;;

let if_not_closed t f =
  match Ivar.peek t.closed with
  | None -> f ()
  | Some { why_closed; _ } ->
    let%map why_closed in
    Or_error.error "tmux was closed" why_closed [%sexp_of: Info.t]
;;

let run t ~command = if_not_closed t (fun () -> run_aux t ~command)
let run_no_output t ~command = if_not_closed t (fun () -> run_no_output_aux t ~command)

let dump_screen_aux ?preserve_trailing_spaces ?dump_escape_sequences ?start ?end_ t =
  if_not_closed t (fun () ->
    let capture_pane =
      [ "capture-pane" ]
      @ (if Option.is_some dump_escape_sequences then [ "-e" ] else [])
      @ (if Option.is_some preserve_trailing_spaces
         then
           [ (* NOTE: This was added in 3.1c according to this:
                https://raw.githubusercontent.com/tmux/tmux/3.1c/CHANGES *)
             "-N"
           ]
         else [])
      @ Option.value_map start ~default:[] ~f:(fun start -> [ "-S"; Int.to_string start ])
      @ Option.value_map end_ ~default:[] ~f:(fun end_ -> [ "-E"; Int.to_string end_ ])
    in
    run t ~command:(capture_pane @ [ ";"; "save-buffer"; "-" ]))
;;

let dump_screen ?preserve_trailing_spaces ?dump_escape_sequences t =
  dump_screen_aux ?preserve_trailing_spaces ?dump_escape_sequences t
;;

let get_scroll_position t =
  if_not_closed t (fun () ->
    let%bind.Deferred.Or_error lines =
      run t ~command:[ "display-message"; "-p"; "-F"; "#{scroll_position}" ]
    in
    match lines with
    | [ "" ] -> Deferred.Or_error.return 0
    | [ line ] -> Deferred.return (Or_error.try_with (fun () -> Int.of_string line))
    | _ ->
      Deferred.Or_error.error_s
        [%message
          "[Tmux.get_scroll_position] error, expected exactly one line when \
           communicating with tmux"
            (lines : string list)])
;;

let dump_visible_screen ?preserve_trailing_spaces ?dump_escape_sequences ~height t =
  let open Deferred.Or_error.Let_syntax in
  let%bind scroll_position = get_scroll_position t in
  dump_screen_aux
    ?preserve_trailing_spaces
    ?dump_escape_sequences
    ~start:(-scroll_position)
    ~end_:(height - scroll_position - 1)
    t
;;

let get_cursor t =
  if_not_closed t (fun () ->
    let format =
      "#{cursor_y},#{cursor_x}:#{cursor_character}"
      (* We expect cursor location and the cursor character to appear like: "1,2:a" means:
         [{ y = 1; x = 2; cursor_character = "a"}] *)
    in
    let%bind.Deferred.Or_error lines =
      run t ~command:[ "display-message"; "-p"; "-F"; format ]
    in
    match lines with
    | [] | _ :: _ :: _ ->
      let n = List.length lines in
      Deferred.Or_error.error_s
        [%message
          "[Tmux.get_cursor] error, expected exactly one line when communicating with \
           tmux, but found"
            (n : int)]
    | [ line ] ->
      Or_error.try_with (fun () ->
        let pos, cursor_character = String.lsplit2_exn line ~on:':' in
        match String.split ~on:',' pos with
        | [ y; x ] ->
          let y, x = Tuple2.map ~f:Int.of_string (y, x) in
          { Cursor.x; y; cursor_character }
        | _ ->
          raise_s
            [%message
              "[get_cursor]: Error when communicated with tmux, expected x,y:char but \
               instead found"
                ~other:(line : string)])
      |> Deferred.return)
;;

let is_ui_empty t =
  let open Deferred.Or_error.Let_syntax in
  let%map lines = dump_screen t in
  match
    List.filter_map lines ~f:(fun line ->
      match String.strip line with
      | "" -> None
      | x -> Some x)
  with
  | [] -> `Yes
  | _ -> `No
;;

let create
  ?(size = Size.default)
  ?don't_wait_for_ui
  ?(support_hyperlinks = true)
  ?(extra_args = [])
  ?working_dir
  ?env
  ?mouse
  (* Note: If we end up wanting anything else other than [working_dir] and [env] to pass
     into [Process.create] consider matching the [Async_unix.Process.create] type. *)
  ~command
  ~name
  ()
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind.Deferred prog = prog ~support_hyperlinks in
  let rundir =
    let base =
      match Sys.getenv "XDG_RUNTIME_DIR", Sys.getenv "TMPDIR" with
      | Some x, _ -> x
      | _, Some x -> x
      | None, None -> "/tmp"
    in
    base ^/ "tmux"
  in
  Core_unix.mkdir_p rundir;
  let t =
    { name =
        (match name with
         | None -> Name.create ~rundir
         | Some n -> n)
    ; prog
    ; extra_args
    ; closed = Ivar.create ()
    ; ownership = Created
    }
  in
  (* The height must change depending on the version of tmux. In version 2.5, the height
     was off by 1.
  *)
  let%bind size =
    let%bind output = Process.run () ~prog ~args:[ "-V" ] in
    let output = String.strip output in
    match String.split ~on:' ' output with
    | [ _tmux; version ] ->
      let version =
        String.take_while version ~f:(function
          | '0' .. '9' | '.' -> true
          | _ -> false)
      in
      (match Or_error.try_with (fun () -> Float.of_string version) with
       | Ok version ->
         if Float.(version >= 2.6)
         then return size
         else return { size with height = Int.( + ) size.height 1 }
       | Error _ as e -> Deferred.return e)
    | _ -> Deferred.Or_error.error_s [%message "Could not parse tmux version" output]
  in
  let command =
    [ "-f" (* We must ignore all system and user tmux configuration *)
    ; "/dev/null"
    ; "new-session"
    ; "-x"
    ; Int.to_string size.width
    ; "-y"
    ; Int.to_string size.height
    ; "-d"
      (* We run the command with /usr/bin/env so that if the command fails to execute for
         some reason, we get output that we can capture (since [env] will actually run;
         whereas if [command] is completely invalid, it will be difficult to capture the
         output from tmux that the command does not exist.) *)
    ; "/usr/bin/env " ^ command
    ; ";"
      (* By specifying the remain-on-exit setting, we will see "Pane is dead" when the
         process exits. This is useful when figuring out why something went wrong (e.g. a
         tmux session ended unexpectedly)
      *)
    ; "set"
    ; "-g"
    ; "remain-on-exit"
    ; ";"
    ; "set-hook"
    ; "pane-died"
    ; sprintf
        "capture-pane -S -32768 ; save-buffer %s ; kill-session"
        (session_end_output_path t)
    ; ";"
    ; "set-window-option"
    ; "-g"
    ; "xterm-keys"
    ; "on"
    ]
    @ (match mouse with
       | None -> []
       | Some () -> [ ";"; "set"; "-g"; "mouse"; "on" ])
    @ [ ";"
        (* By specifying the wait-for command (that shouldn't ever get signalled), we wait
           until the session dies *)
      ; "wait-for"
      ; "some-random-channel-name-that-people-should-not-use"
      ]
  in
  let args = gen_args t ~command in
  let%bind process = Process.create ~prog ~args ?env ?working_dir () in
  let exe = prog :: args in
  don't_wait_for
    (let open Deferred.Let_syntax in
     let%map output = Process.collect_output_and_wait process in
     match Ivar.peek t.closed with
     | Some _ -> () (* [close] was already called, don't overwrite *)
     | None ->
       let result =
         let%bind screen =
           Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
             Reader.with_file (session_end_output_path t) ~f:Reader.contents)
         in
         (* this is what will be returned on subsequent calls to [close] *)
         return
           (Or_error.error_s
              [%message
                "tmux process already exited (perhaps underlying command died)"
                  (exe : string list)
                  (screen : string Or_error.t)])
       in
       let why_closed =
         let%map pane = last_screen t in
         (* this will be returned (as an error) by subsequent calls to [run] or
            [run_no_output] *)
         [%message
           "tmux process already exited"
             (output : Process.Output.t)
             (exe : string list)
             (pane : string list Or_error.t)]
         |> Info.t_of_sexp
       in
       Ivar.fill_if_empty t.closed { result; why_closed });
  let%bind () = wait_for_socket_existence ~timeout:(sec 30.) t in
  let%bind () =
    match don't_wait_for_ui with
    | Some () -> Deferred.Or_error.ok_unit
    | None ->
      let open Deferred.Let_syntax in
      let generous_timeout = Clock.after (sec 30.) in
      Deferred.repeat_until_finished () (fun () ->
        match Deferred.peek generous_timeout with
        | Some () ->
          return
            (`Finished
              (Or_error.error_s [%message "Timed out waiting for UI to be non-empty"]))
        | None ->
          (match%bind is_ui_empty t with
           | Ok `Yes ->
             let%map () = Clock.after looping_pause_time in
             `Repeat ()
           | Ok `No -> return (`Finished (Ok ()))
           | Error e -> return (`Finished (Error e))))
  in
  return t
;;

module For_testing = struct
  module Name = struct
    include Name

    let create () = create ~rundir:"/tmp"
  end

  let create
    ?size
    ?don't_wait_for_ui
    ?support_hyperlinks
    ?extra_args
    ?working_dir
    ?env
    ?mouse
    ~command
    ~name
    ()
    =
    create
      ?size
      ?don't_wait_for_ui
      ?support_hyperlinks
      ?extra_args
      ?working_dir
      ?env
      ?mouse
      ~command
      ~name:(Some name)
      ()
  ;;

  let save_buffer_path = session_end_output_path
  let socket_path = socket_path
  let tmux_executable_path = prog
  let name t = t.name
end

let create
  ?size
  ?don't_wait_for_ui
  ?support_hyperlinks
  ?extra_args
  ?working_dir
  ?env
  ?mouse
  ~command
  ()
  =
  create
    ?size
    ?don't_wait_for_ui
    ?support_hyperlinks
    ?extra_args
    ?working_dir
    ?env
    ?mouse
    ~command
    ~name:None
    ()
;;

let attach { Session_id.name; prog; extra_args } =
  let open Deferred.Or_error.Let_syntax in
  let client = Ivar.create () in
  let t =
    { name
    ; prog
    ; extra_args
    ; closed = Ivar.create ()
    ; ownership = Attached { client = Ivar.read client }
    }
  in
  (* The socket should already exist, so we don't need a timeout. *)
  let%bind () = wait_for_socket_existence ~timeout:(sec 0.) t in
  (* Spawn a tmux client that blocks on [wait-for] so we can detect when the session dies.
     We wait until the socket exists before starting the client. If process creation
     fails, the ivar is never filled — [close] and the session-death monitor simply never
     fire. *)
  let args =
    gen_args
      t
      ~command:[ "wait-for"; "some-random-channel-name-that-people-should-not-use" ]
  in
  let%bind process = Process.create ~prog ~args () in
  Ivar.fill_exn client process;
  don't_wait_for
    (let open Deferred.Let_syntax in
     let%map output = Process.collect_output_and_wait process in
     match Ivar.peek t.closed with
     | Some _ -> ()
     | None ->
       let screen = last_screen t in
       let result =
         let%map screen in
         Or_error.error_s
           [%message
             "tmux session died (detected by attached process)"
               (screen : string list Or_error.t)]
       in
       let why_closed =
         let%map screen in
         [%message
           "tmux session died"
             (output : Process.Output.t)
             (screen : string list Or_error.t)]
         |> Info.t_of_sexp
       in
       Ivar.fill_if_empty t.closed { result; why_closed });
  return t
;;

let tmux_key_of_jane_key_basic (x : Jane_term_types.Key.basic) =
  match x with
  | `Char ';' ->
    (* NOTE: tmux treats a bare ";" argument as a command separator. We escape semicolons
       as "\\;" when sending so that the underlying program can receive semicolons. *)
    "\\;"
  | `Char c -> String.make 1 c
  | `Enter -> "Enter"
  | `Down -> "Down"
  | `Up -> "Up"
  | `Left -> "Left"
  | `Right -> "Right"
  | `Page_up -> "PageUp"
  | `Page_down -> "PageDown"
  | `Home -> "Home"
  | `End -> "End"
  | `F n -> Printf.sprintf "F%i" n
  | `Backspace -> "BSpace"
  | `Tab -> "Tab"
  | `Shift_tab -> "BTab"
  | `Shift_up -> "S-Up"
  | `Shift_down -> "S-Down"
  | `Shift_left -> "S-Left"
  | `Shift_right -> "S-Right"
  | `Esc -> "Escape"
  | `Insert -> "IC"
  | `Delete -> "DC"
  | `Strange i -> failwithf "Unknown strange character, code %d" i ()
  | `Ctrl c ->
    let s =
      match c with
      | `a -> "a"
      | `b -> "b"
      | `c -> "c"
      | `d -> "d"
      | `e -> "e"
      | `f -> "f"
      | `g -> "g"
      | `j -> "j"
      | `k -> "k"
      | `l -> "l"
      | `n -> "n"
      | `o -> "o"
      | `p -> "p"
      | `q -> "q"
      | `r -> "r"
      | `s -> "s"
      | `t -> "t"
      | `u -> "u"
      | `v -> "v"
      | `w -> "w"
      | `x -> "x"
      | `y -> "y"
      | `z -> "z"
    in
    "C-" ^ s
;;

let tmux_key_of_jane_key (x : Jane_term_types.Key.t) =
  match x with
  | #Jane_term_types.Key.basic as x -> tmux_key_of_jane_key_basic x
  | `Alt x -> "M-" ^ tmux_key_of_jane_key_basic x
;;

let send_keys_raw t (keys : string list) =
  List.chunks_of keys ~length:1000
  |> Deferred.Or_error.List.iter ~how:`Sequential ~f:(fun keys ->
    run_no_output t ~command:("send-keys" :: keys))
;;

let tmux_string_of_key : Key.t -> string = function
  | #Jane_term_types.Key.t as key -> tmux_key_of_jane_key key
  | `Raw_escape s -> s
  | `Paste `Start -> "\027[200~"
  | `Paste `End -> "\027[201~"
;;

let send_keys t (keys : Key.t list) =
  let keys = List.map keys ~f:tmux_string_of_key in
  send_keys_raw t keys
;;

let send_key t key = send_keys t [ key ]

let send_chars t string =
  send_keys t (String.to_list string |> List.map ~f:(fun c -> `Char c))
;;

let scroll t direction =
  let scroll_command =
    match direction with
    | `Up -> "scroll-up"
    | `Down -> "scroll-down"
  in
  let scroll_in_copy_mode = [%string "send-keys -X -N 5 %{scroll_command}"] in
  match direction with
  | `Up ->
    run_no_output
      t
      ~command:
        [ "if-shell"
        ; "-F"
        ; "#{pane_in_mode}"
        ; scroll_in_copy_mode
        ; [%string "copy-mode -e; %{scroll_in_copy_mode}"]
        ]
  | `Down ->
    run_no_output t ~command:[ "if-shell"; "-F"; "#{pane_in_mode}"; scroll_in_copy_mode ]
;;

let resize t (new_size : Size.t) =
  run_no_output
    t
    ~command:
      [ "resize-window"
      ; "-x"
      ; Int.to_string new_size.width
      ; "-y"
      ; Int.to_string new_size.height
      ]
;;

let with_command
  ?size
  ?don't_wait_for_ui
  ?support_hyperlinks
  ?extra_args
  ?working_dir
  ?env
  ?mouse
  ~command
  ~f
  ()
  =
  match%bind
    create
      ?size
      ?don't_wait_for_ui
      ?support_hyperlinks
      ?extra_args
      ?working_dir
      ?env
      ?mouse
      ~command
      ()
  with
  | Error _ as e -> return e
  | Ok t ->
    (match%bind
       Deferred.Or_error.try_with_join ~run:`Schedule ~rest:`Log (fun () ->
         let%bind result = f t in
         match%map close t with
         | Ok (_ : Close_result.t) -> Ok result
         | Error e -> Error e)
     with
     | Ok result -> return (Ok result)
     | Error command_error ->
       if is_closed t
       then return (Error command_error)
       else
         let open Deferred.Let_syntax in
         (match%map close t with
          | Ok _ -> Error command_error
          | Error close_error -> Error (Error.of_list [ command_error; close_error ])))
;;

let close t = Deferred.Or_error.ignore_m @@ close t

let pane_pid t =
  let open Deferred.Or_error.Let_syntax in
  let%bind lines = run t ~command:[ "display-message"; "-p"; "#{pane_pid}" ] in
  match lines with
  | [ pid_str ] ->
    (match Int.of_string_opt (String.strip pid_str) with
     | Some pid -> return (Pid.of_int pid)
     | None ->
       Deferred.Or_error.error_s [%message "Failed to parse pane PID" (pid_str : string)])
  | _ ->
    Deferred.Or_error.error_s
      [%message "Expected exactly one line from display-message" (lines : string list)]
;;

let closed t =
  let%map (_ : closed) = Ivar.read t.closed in
  ()
;;
