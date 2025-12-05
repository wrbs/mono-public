open! Core
open! Midi

let%expect_test "Single messages roundtrip" =
  Quickcheck.test
    [%quickcheck.generator: Live_message.t]
    ~sexp_of:[%sexp_of: Live_message.t]
    ~f:(fun message ->
      let parsed = message |> Live_message.to_string |> Live_message.parse_string in
      [%test_eq: Live_message.t iarray] [: message :] parsed)
;;

let%expect_test "Show running status encoding" =
  let show_msg m = print_s [%sexp (Live_message.to_string m : Byte.String.t)] in
  let show_msgs ms =
    print_s [%sexp (Live_message.to_string_many ms ~running_status:true : Byte.String.t)]
  in
  let velocity = Value.encode Float 0.5 in
  let note1 = Value.of_int_exn 45 in
  let note2 = Value.of_int_exn 50 in
  let a = Live_message.note_on note1 ~velocity ~channel:C1 in
  show_msg a;
  [%expect {| (80 2D 40) |}];
  let b = Live_message.note_on note2 ~velocity ~channel:C1 in
  show_msg b;
  [%expect {| (80 32 40) |}];
  let c = Live_message.note_on note1 ~velocity ~channel:C2 in
  show_msg c;
  [%expect {| (81 2D 40) |}];
  let d = Live_message.note_off note2 ~channel:C1 in
  show_msg d;
  [%expect {| (90 32 00) |}];
  (* Now combinations *)
  show_msgs [: a; b :];
  [%expect {| (80 2D 40 32 40) |}];
  show_msgs [: a; d; b :];
  [%expect {| (80 2D 40 90 32 00 80 32 40) |}];
  show_msgs [: a; c; b :];
  [%expect {| (80 2D 40 81 2D 40 80 32 40) |}]
;;

let check_many ~running_status =
  Quickcheck.test
    [%quickcheck.generator: Live_message.t Iarray.t]
    ~sexp_of:[%sexp_of: Live_message.t Iarray.t]
    ~f:(fun messages ->
      let parsed =
        messages
        |> Live_message.to_string_many ~running_status
        |> Live_message.parse_string
      in
      [%test_eq: Live_message.t iarray] messages parsed)
;;

let%expect_test "Running status roundtrips" = check_many ~running_status:true

let%expect_test "Many roundtrips without running status" =
  check_many ~running_status:false
;;
