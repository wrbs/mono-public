open! Core

let%expect_test "Reads should run, then writes, in the order they were registered" =
  let () = Fastdom.write ~f:(fun () -> print_endline "Third") in
  let () = Fastdom.read ~f:(fun () -> print_endline "First") in
  let () =
    Fastdom.read_then_write
      ~read:(fun () -> print_endline "Second")
      ~write:(fun () -> print_endline "Fourth")
  in
  let () = Fastdom.Expert.run_actions () in
  [%expect
    {|
    First
    Second
    Third
    Fourth
    |}]
;;

let%expect_test "Queue should be empty after running reads" =
  let () = Fastdom.write ~f:(fun () -> print_endline "Third") in
  let () = Fastdom.read ~f:(fun () -> print_endline "First") in
  let () =
    Fastdom.read_then_write
      ~read:(fun () -> print_endline "Second")
      ~write:(fun () -> print_endline "Fourth")
  in
  let (`Run_writes run_writes) = Fastdom.Expert.run_reads () in
  let () =
    [%expect
      {|
      First
      Second
      |}]
  in
  let ( (* Should be 0 *) ) =
    Fastdom.For_testing.num_elements_in_queue () |> Int.to_string |> print_endline
  in
  let () = [%expect {| 0 |}] in
  let () = run_writes () in
  [%expect
    {|
    Third
    Fourth
    |}]
;;

let%expect_test "Should not be able to add another write to [run_writes] until next \
                 [run_actions]"
  =
  let () = Fastdom.write ~f:(fun () -> print_endline "Third") in
  let () = Fastdom.read ~f:(fun () -> print_endline "First") in
  let () =
    Fastdom.read_then_write
      ~read:(fun () -> print_endline "Second")
      ~write:(fun () -> print_endline "Fourth")
  in
  let (`Run_writes run_writes) = Fastdom.Expert.run_reads () in
  let () =
    [%expect
      {|
      First
      Second
      |}]
  in
  let () = Fastdom.write ~f:(fun () -> print_endline "Fifth") in
  let () = run_writes () in
  let () =
    [%expect
      {|
      Third
      Fourth
      |}]
  in
  let () = Fastdom.Expert.run_actions () in
  [%expect {| Fifth |}]
;;
