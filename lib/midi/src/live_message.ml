open! Core

type t =
  | MIDI of Message.With_channel.t
  | Sysex of Value.t Iarray.t
  | Mtc_quarter_frame of Value.t
  | Song_position of Value.Double.t
  | Song_select of Value.t
  | Tune_request
  | Realtime of Status.Realtime.t
[@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

let note_on note ~velocity ~channel = MIDI (channel, Note_on { note; velocity })

let note_off ?(velocity = Value.min_value) note ~channel =
  MIDI (channel, Note_off { note; velocity })
;;

let aftertouch note ~pressure ~channel = MIDI (channel, Aftertouch { note; pressure })
let cc value ~controller ~channel = MIDI (channel, Controller { value; controller })
let program_change value ~channel = MIDI (channel, Program_change value)
let pressure value ~channel = MIDI (channel, Pressure value)
let pitch_wheel value ~channel = MIDI (channel, Pitch_wheel value)
let sysex values = Sysex values
let mtc_quarter_frame value = Mtc_quarter_frame value
let song_position value = Song_position value
let song_select value = Song_select value
let tune_request = Tune_request
let clock = Realtime Clock
let tick = Realtime Tick
let start = Realtime Start
let continue = Realtime Continue
let stop = Realtime Stop
let active_sense = Realtime Active_sense
let reset = Realtime Reset

let payload_length = function
  | MIDI (_, cmd) -> Message.payload_length cmd
  | Sysex payload -> Iarray.length payload + 1
  | Mtc_quarter_frame _ -> 1
  | Song_position _ -> 2
  | Song_select _ -> 1
  | Tune_request -> 0
  | Realtime _ -> 0
;;

let length t = 1 + payload_length t

let start_status : t -> Status.t = function
  | MIDI (channel, cmd) -> MIDI (channel, Message.kind cmd)
  | Sysex _ -> Sysex
  | Mtc_quarter_frame _ -> Mtc_quarter_frame
  | Song_position _ -> Song_position
  | Song_select _ -> Song_select
  | Tune_request -> Tune_request
  | Realtime rt -> Realtime rt
;;

let encode_midi_payload message ~f =
  let a, b = Message.payload message in
  f a;
  Option.iter b ~f
;;

let encode' t ~f =
  let value v = f (Parsed_byte.Value v) in
  f (Status (start_status t));
  match t with
  | MIDI (_, cmd) -> encode_midi_payload cmd ~f:value
  | Sysex values ->
    Iarray.iter values ~f:value;
    f (Status End_sysex)
  | Mtc_quarter_frame v -> value v
  | Song_position v ->
    let ~hi, ~lo = Value.Double.to_values v in
    value hi;
    value lo
  | Song_select v -> value v
  | Tune_request | Realtime _ -> ()
;;

let encode t ~f = encode' t ~f:(fun b -> f (Parsed_byte.to_byte b))

let make_iarray_res inner ~init ~len =
  let buffer = Array.create init ~len in
  let ptr = ref 0 in
  let f x =
    buffer.(!ptr) <- x;
    ptr := !ptr + 1
  in
  let res = inner ~f in
  Iarray.unsafe_of_array__promise_no_mutation buffer, res
;;

let make_string_res inner ~len =
  let buffer = Bytes.create len in
  let ptr = ref 0 in
  let f x =
    Bytes.set buffer !ptr x;
    ptr := !ptr + 1
  in
  let res = inner ~f in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buffer, res
;;

let to_iarray t =
  let x, () = make_iarray_res (encode' t) ~init:(Value Value.min_value) ~len:(length t) in
  x
;;

let to_string t =
  let x, () = make_string_res (encode t) ~len:(length t) in
  x
;;

module Running_status = struct
  type t = Message.Status.t option [@@deriving equal ~localize, sexp_of]

  let initial = None

  let length' t msg =
    match msg with
    | MIDI (channel, cmd) ->
      let kind = Message.kind cmd in
      let pl = Message.payload_length cmd in
      let length =
        match [%equal: t] t (Some (channel, kind)) with
        | true -> pl
        | false -> 1 + pl
      in
      length, Some (channel, kind)
    | _ -> length msg, None
  ;;

  let length t msg = length' t msg |> Tuple2.get1

  let encode' t msg ~f =
    match msg with
    | MIDI (channel, message) ->
      let kind = Message.kind message in
      let () =
        match [%equal: t] t (Some (channel, kind)) with
        | false -> encode' msg ~f
        | true -> encode_midi_payload message ~f:(fun v -> f (Value v))
      in
      Some (channel, kind)
    | _ ->
      encode' msg ~f;
      None
  ;;

  let encode t msg ~f = encode' t msg ~f:(fun b -> f (Parsed_byte.to_byte b))

  let to_iarray t msg =
    make_iarray_res (encode' t msg) ~init:(Value Value.min_value) ~len:(length t msg)
  ;;

  let to_string t msg = make_string_res (encode t msg) ~len:(length t msg)

  let to_string_many t msgs =
    let len, _ =
      Iarray.fold msgs ~init:(0, t) ~f:(fun (acc, t) msg ->
        let x, t' = length' t msg in
        acc + x, t')
    in
    make_string_res ~len (fun ~f ->
      Iarray.fold msgs ~init:t ~f:(fun t msg -> encode t msg ~f))
  ;;
end

let to_string_many msgs ~running_status =
  match running_status with
  | true ->
    let s, _ = Running_status.to_string_many Running_status.initial msgs in
    s
  | false ->
    let len = Iarray.sum (module Int) msgs ~f:length in
    let s, () = make_string_res (fun ~f -> Iarray.iter msgs ~f:(encode ~f)) ~len in
    s
;;

module Parser = struct
  module State = struct
    type t =
      | Initial
      | MIDI_1 of Channel.t * Message.Kind.Single.t
      | MIDI_2 of Channel.t * Message.Kind.Double.t * Value.t option
      | Sysex
      | Mtc_quarter_frame
      | Song_position of Value.t option
      | Song_select
    [@@deriving sexp_of]

    let add t (byte : Parsed_byte.t) =
      let emit, t' =
        match byte with
        | Status status ->
          (match status with
           | MIDI (channel, kind) ->
             (match Message.Kind.to_typed kind with
              | T (Single x) -> `cont, MIDI_1 (channel, x)
              | T (Double x) -> `cont, MIDI_2 (channel, x, None))
           | Sysex -> `cont, Sysex
           | End_sysex -> `emit_sysex, Initial
           | Mtc_quarter_frame -> `cont, Mtc_quarter_frame
           | Song_position -> `cont, Song_position None
           | Song_select -> `cont, Song_select
           | Tune_request -> `emit Tune_request, Initial
           | Realtime rt -> `emit (Realtime rt), t
           | U_F4 | U_F5 -> `cont, Initial)
        | Value v ->
          (match t with
           | Initial -> `cont, Initial
           | MIDI_1 (channel, k) ->
             let message = Message.decode_payload (Single k) v in
             `emit (MIDI (channel, message)), MIDI_1 (channel, k)
           | MIDI_2 (channel, c, None) -> `cont, MIDI_2 (channel, c, Some v)
           | MIDI_2 (channel, k, Some v1) ->
             let message = Message.decode_payload (Double k) (v1, v) in
             `emit (MIDI (channel, message)), MIDI_2 (channel, k, None)
           | Sysex -> `cont, Sysex
           | Mtc_quarter_frame -> `emit (Mtc_quarter_frame v), Initial
           | Song_position None -> `cont, Song_position (Some v)
           | Song_position (Some v1) ->
             let value = Value.Double.of_values ~hi:v1 ~lo:v in
             `emit (Song_position value), Initial
           | Song_select -> `emit (Song_select v), Initial)
      in
      let sysex =
        match t with
        | Sysex ->
          (match byte with
           | Status (Realtime _) -> None
           | Status _ -> Some `clear
           | Value v -> Some (`push v))
        | _ -> None
      in
      t', ~emit, ~sysex
    ;;
  end

  module Immutable = struct
    type t =
      { state : State.t
      ; sysex_rev : Value.t list
      }
    [@@deriving sexp_of]

    let initial = { state = Initial; sysex_rev = [] }

    let add' t byte =
      let state, ~emit, ~sysex = State.add t.state byte in
      let command =
        match emit with
        | `cont -> None
        | `emit_sysex ->
          let values = Iarray.of_list_rev t.sysex_rev in
          Some (Sysex values)
        | `emit command -> Some command
      in
      let sysex_rev =
        match sysex with
        | None -> t.sysex_rev
        | Some (`push v) -> v :: t.sysex_rev
        | Some `clear -> []
      in
      command, { state; sysex_rev }
    ;;

    let add t byte = add' t (Parsed_byte.of_byte byte)
  end

  type t =
    { mutable state : State.t
    ; sysex : Value.t Vec.t
    }
  [@@deriving sexp_of]

  let create () = { state = Initial; sysex = Vec.create () }

  let add' t byte =
    let state', ~emit, ~sysex = State.add t.state byte in
    let command =
      match emit with
      | `cont -> None
      | `emit_sysex ->
        let values = Vec.to_iarray t.sysex in
        Some (Sysex values)
      | `emit command -> Some command
    in
    Option.iter sysex ~f:(function
      | `push v -> Vec.push_back t.sysex v
      | `clear -> Vec.clear t.sysex);
    t.state <- state';
    command
  ;;

  let add t byte = add' t (Parsed_byte.of_byte byte)
end

let parse_string s =
  let parser = Parser.create () in
  let output = Vec.create () in
  String.iter s ~f:(fun c ->
    Parser.add parser c |> Option.iter ~f:(fun t -> Vec.push_back output t));
  Vec.to_iarray output
;;
