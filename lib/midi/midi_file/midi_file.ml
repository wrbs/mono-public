open! Core
module U15 = Num.U15
module U24 = Num.U24
module U28 = Num.U28
module Event = Event
module Smtpe = Smtpe
module Header = Header
module Io = Io

type t =
  { header : Header.t
  ; tracks : Event.t iarray iarray
  }
[@@deriving sexp_of]

module Parse = struct
  open Result.Let_syntax
  open Io.Read

  let chunk ~next =
    let%map a = byte ~next
    and b = byte ~next
    and c = byte ~next
    and d = byte ~next
    and len = u32 ~next in
    (a, b, c, d), len
  ;;

  let guard cond msg = if cond then Ok () else Or_error.error_s msg

  let rec parse_track_header ~next =
    match%bind chunk ~next with
    | ('M', 'T', 'r', 'k'), len -> Ok len
    | _, len ->
      let%bind () = skip len ~next in
      parse_track_header ~next
  ;;

  let parse_header ~next =
    match%bind chunk ~next with
    | ('M', 'T', 'h', 'd'), len ->
      let%bind () = guard (len >= 6) [%message "Header too short" (len : int)] in
      let%bind format =
        match%bind u16 ~next with
        | 0 -> Ok Header.Format.Single_track
        | 1 -> Ok Header.Format.Parallel
        | 2 -> Ok Header.Format.Sequential
        | f -> error_s [%message "Unexpected header format" (f : int)]
      in
      let%bind num_tracks = u16 ~next in
      let%bind tc1 = i8 ~next in
      let%bind tc2 = byte ~next in
      let%map timing =
        if tc1 >= 0
        then (
          let value = (tc1 lsl 8) lor Byte.to_int tc2 |> Num.U15.of_int_exn in
          return (Header.Timing.Metrical value))
        else (
          let%map fps =
            match Smtpe.Fps.of_int (-tc1) with
            | Some tc -> Ok tc
            | None -> Or_error.error_s [%message "Unexpected fps" (tc1 : int)]
          in
          Header.Timing.Timecode (fps, tc2))
      in
      { Header.format; timing }, ~num_tracks
    | got_bytes, len ->
      Or_error.error_s
        [%message "Expected header" (got_bytes : char * char * char * char) (len : int)]
  ;;

  let parse ~collectors ~next =
    with_offset_in_error ~next
    @@ fun ~next ->
    let%bind header, ~num_tracks = parse_header ~next in
    let ~events:get_track, ~tracks = collectors header in
    let rec aux remaining ~tracks =
      if remaining <= 0
      then Ok (Collector.finish tracks)
      else (
        let%bind len = parse_track_header ~next in
        let%bind track =
          subchunk ~len ~next (fun ~at_end ~next ->
            let rec loop ~track ~running_status =
              match at_end () with
              | true -> Ok (Collector.finish track)
              | false ->
                let%bind event, ~running_status = Event.read ~next ~running_status in
                let track = Collector.add track event in
                loop ~track ~running_status
            in
            loop ~track:(get_track (num_tracks - remaining)) ~running_status:None)
        in
        aux (remaining - 1) ~tracks:(Collector.add tracks track))
    in
    aux num_tracks ~tracks
  ;;
end

let parse = Parse.parse

let read =
  parse ~collectors:(fun header ->
    ( ~events:(fun _ -> Collector.iarray ())
    , ~tracks:(Collector.iarray ()
               |> Collector.Output.map ~f:(fun tracks -> { header; tracks })) ))
;;

let of_string s = Io.read_string s read
