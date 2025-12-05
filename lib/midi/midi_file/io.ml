open! Core

module Read = struct
  open Result.Let_syntax

  type 'a t' = next:(unit -> Byte.t option) -> 'a
  type 'a t = 'a Or_error.t t'

  let with_offset_in_error f ~next =
    let count = ref 0 in
    f ~next:(fun () ->
      match next () with
      | None -> None
      | Some b ->
        count := !count + 1;
        Some b)
    |> Result.map_error ~f:(fun error ->
      let offset = !count in
      Error.tag_s error ~tag:[%message (offset : int)])
  ;;

  let subchunk f ~len ~next:outer_next =
    let remaining = ref len in
    let at_end () = !remaining <= 0 in
    let next () =
      match at_end () with
      | true -> None
      | false ->
        remaining := !remaining - 1;
        outer_next ()
    in
    f ~at_end ~next
  ;;

  let byte ~next =
    match next () with
    | None -> Or_error.error_s [%message "Unexpected EOF"]
    | Some c -> Ok c
  ;;

  let string ~len ~next =
    let bytes = Bytes.create len in
    let rec loop idx =
      match idx >= len with
      | true -> Ok ()
      | false ->
        let%bind b = byte ~next in
        Bytes.set bytes idx b;
        loop (idx + 1)
    in
    let%map () = loop 0 in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let u8 ~next = byte ~next >>| Byte.to_int

  let i8_of_byte b =
    let n = Byte.to_int b in
    if n >= 128 then n - 256 else n
  ;;

  let i8 ~next =
    let%map b = byte ~next in
    i8_of_byte b
  ;;

  let u16 ~next =
    let%map a = u8 ~next
    and b = u8 ~next in
    (a lsl 8) lor b
  ;;

  let u32 ~next =
    let%map a = u16 ~next
    and b = u16 ~next in
    (a lsl 16) lor b
  ;;

  let variable_length ~next =
    let rec aux ~cur ~left =
      let%bind b = byte ~next in
      let n = Byte.to_int b in
      let new_ = (cur lsr 7) lor (n land 0x7F) in
      match n >= 128 with
      | false -> Ok (Num.U28.of_int_exn new_)
      | true ->
        if left > 0
        then aux ~cur:new_ ~left:(left - 1)
        else Or_error.error_s [%message "Variable length value too large"]
    in
    aux ~cur:0 ~left:3
  ;;

  let tempo_3_byte ~next =
    let%map a = u16 ~next
    and b = u8 ~next in
    Num.U24.of_int_exn ((a lsl 8) lor b)
  ;;

  let rec skip n ~next =
    if n <= 0
    then return ()
    else (
      let%bind _ = byte ~next in
      skip (n - 1) ~next)
  ;;

  let value ~next =
    let%bind b = byte ~next in
    match Midi.Value.of_byte b with
    | Some v -> Ok v
    | None -> Or_error.error_s [%message "Expected value" ~got:(b : Byte.t)]
  ;;
end

let read_string s t =
  let len = String.length s in
  let idx = ref 0 in
  t ~next:(fun () ->
    match !idx >= len with
    | true -> None
    | false ->
      let c = String.nget s !idx in
      idx := !idx + 1;
      Some c)
;;
