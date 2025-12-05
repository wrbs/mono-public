open! Core
open! Midi

let%expect_test "Every parsed byte roundtrips" =
  Parsed_byte.all
  |> List.iter ~f:(fun x ->
    let x' = x |> Parsed_byte.to_byte |> Parsed_byte.of_byte in
    [%test_eq: Parsed_byte.t] x x')
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (vendor/ppx_assert/runtime-lib/runtime.ml.E "comparison failed"
    ((Status (MIDI (1 Note_on))) vs (Status (MIDI (1 Note_off)))
      (Loc lib/midi/test/test_parsed_bytes.ml:8:15)))
  Raised at Ppx_assert_lib__Runtime.test_eq__stack in file "vendor/ppx_assert/runtime-lib/runtime.ml", line 116, characters 22-69
  Called from Base__List0.iter__bits64__local.loop in file "vendor/base/src/list0.ml" (inlined), line 99, characters 6-9
  Called from Base__List0.iter__bits64__local in file "vendor/base/src/list0.ml", line 102, characters 2-11
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "vendor/ppx_expect/runtime/test_block.ml", line 358, characters 10-25
  |}]
;;

let%expect_test "Show parsed bytes" =
  Expectable.print
    (List.map Parsed_byte.all ~f:(fun parsed ->
       let byte = Parsed_byte.to_byte parsed in
       [%message (byte : Byte.t) (parsed : Parsed_byte.t)]));
  [%expect {|
    ┌──────┬─────────────────────────────────────┐
    │ byte │ parsed                              │
    ├──────┼─────────────────────────────────────┤
    │ 00   │ (Value 0)                           │
    │ 01   │ (Value 1)                           │
    │ 02   │ (Value 2)                           │
    │ 03   │ (Value 3)                           │
    │ 04   │ (Value 4)                           │
    │ 05   │ (Value 5)                           │
    │ 06   │ (Value 6)                           │
    │ 07   │ (Value 7)                           │
    │ 08   │ (Value 8)                           │
    │ 09   │ (Value 9)                           │
    │ 0A   │ (Value 10)                          │
    │ 0B   │ (Value 11)                          │
    │ 0C   │ (Value 12)                          │
    │ 0D   │ (Value 13)                          │
    │ 0E   │ (Value 14)                          │
    │ 0F   │ (Value 15)                          │
    │ 10   │ (Value 16)                          │
    │ 11   │ (Value 17)                          │
    │ 12   │ (Value 18)                          │
    │ 13   │ (Value 19)                          │
    │ 14   │ (Value 20)                          │
    │ 15   │ (Value 21)                          │
    │ 16   │ (Value 22)                          │
    │ 17   │ (Value 23)                          │
    │ 18   │ (Value 24)                          │
    │ 19   │ (Value 25)                          │
    │ 1A   │ (Value 26)                          │
    │ 1B   │ (Value 27)                          │
    │ 1C   │ (Value 28)                          │
    │ 1D   │ (Value 29)                          │
    │ 1E   │ (Value 30)                          │
    │ 1F   │ (Value 31)                          │
    │ 20   │ (Value 32)                          │
    │ 21   │ (Value 33)                          │
    │ 22   │ (Value 34)                          │
    │ 23   │ (Value 35)                          │
    │ 24   │ (Value 36)                          │
    │ 25   │ (Value 37)                          │
    │ 26   │ (Value 38)                          │
    │ 27   │ (Value 39)                          │
    │ 28   │ (Value 40)                          │
    │ 29   │ (Value 41)                          │
    │ 2A   │ (Value 42)                          │
    │ 2B   │ (Value 43)                          │
    │ 2C   │ (Value 44)                          │
    │ 2D   │ (Value 45)                          │
    │ 2E   │ (Value 46)                          │
    │ 2F   │ (Value 47)                          │
    │ 30   │ (Value 48)                          │
    │ 31   │ (Value 49)                          │
    │ 32   │ (Value 50)                          │
    │ 33   │ (Value 51)                          │
    │ 34   │ (Value 52)                          │
    │ 35   │ (Value 53)                          │
    │ 36   │ (Value 54)                          │
    │ 37   │ (Value 55)                          │
    │ 38   │ (Value 56)                          │
    │ 39   │ (Value 57)                          │
    │ 3A   │ (Value 58)                          │
    │ 3B   │ (Value 59)                          │
    │ 3C   │ (Value 60)                          │
    │ 3D   │ (Value 61)                          │
    │ 3E   │ (Value 62)                          │
    │ 3F   │ (Value 63)                          │
    │ 40   │ (Value 64)                          │
    │ 41   │ (Value 65)                          │
    │ 42   │ (Value 66)                          │
    │ 43   │ (Value 67)                          │
    │ 44   │ (Value 68)                          │
    │ 45   │ (Value 69)                          │
    │ 46   │ (Value 70)                          │
    │ 47   │ (Value 71)                          │
    │ 48   │ (Value 72)                          │
    │ 49   │ (Value 73)                          │
    │ 4A   │ (Value 74)                          │
    │ 4B   │ (Value 75)                          │
    │ 4C   │ (Value 76)                          │
    │ 4D   │ (Value 77)                          │
    │ 4E   │ (Value 78)                          │
    │ 4F   │ (Value 79)                          │
    │ 50   │ (Value 80)                          │
    │ 51   │ (Value 81)                          │
    │ 52   │ (Value 82)                          │
    │ 53   │ (Value 83)                          │
    │ 54   │ (Value 84)                          │
    │ 55   │ (Value 85)                          │
    │ 56   │ (Value 86)                          │
    │ 57   │ (Value 87)                          │
    │ 58   │ (Value 88)                          │
    │ 59   │ (Value 89)                          │
    │ 5A   │ (Value 90)                          │
    │ 5B   │ (Value 91)                          │
    │ 5C   │ (Value 92)                          │
    │ 5D   │ (Value 93)                          │
    │ 5E   │ (Value 94)                          │
    │ 5F   │ (Value 95)                          │
    │ 60   │ (Value 96)                          │
    │ 61   │ (Value 97)                          │
    │ 62   │ (Value 98)                          │
    │ 63   │ (Value 99)                          │
    │ 64   │ (Value 100)                         │
    │ 65   │ (Value 101)                         │
    │ 66   │ (Value 102)                         │
    │ 67   │ (Value 103)                         │
    │ 68   │ (Value 104)                         │
    │ 69   │ (Value 105)                         │
    │ 6A   │ (Value 106)                         │
    │ 6B   │ (Value 107)                         │
    │ 6C   │ (Value 108)                         │
    │ 6D   │ (Value 109)                         │
    │ 6E   │ (Value 110)                         │
    │ 6F   │ (Value 111)                         │
    │ 70   │ (Value 112)                         │
    │ 71   │ (Value 113)                         │
    │ 72   │ (Value 114)                         │
    │ 73   │ (Value 115)                         │
    │ 74   │ (Value 116)                         │
    │ 75   │ (Value 117)                         │
    │ 76   │ (Value 118)                         │
    │ 77   │ (Value 119)                         │
    │ 78   │ (Value 120)                         │
    │ 79   │ (Value 121)                         │
    │ 7A   │ (Value 122)                         │
    │ 7B   │ (Value 123)                         │
    │ 7C   │ (Value 124)                         │
    │ 7D   │ (Value 125)                         │
    │ 7E   │ (Value 126)                         │
    │ 90   │ (Status (MIDI (1 Note_on)))         │
    │ 91   │ (Status (MIDI (2 Note_on)))         │
    │ 92   │ (Status (MIDI (3 Note_on)))         │
    │ 93   │ (Status (MIDI (4 Note_on)))         │
    │ 94   │ (Status (MIDI (5 Note_on)))         │
    │ 95   │ (Status (MIDI (6 Note_on)))         │
    │ 96   │ (Status (MIDI (7 Note_on)))         │
    │ 97   │ (Status (MIDI (8 Note_on)))         │
    │ 98   │ (Status (MIDI (9 Note_on)))         │
    │ 99   │ (Status (MIDI (10 Note_on)))        │
    │ 9A   │ (Status (MIDI (11 Note_on)))        │
    │ 9B   │ (Status (MIDI (12 Note_on)))        │
    │ 9C   │ (Status (MIDI (13 Note_on)))        │
    │ 9D   │ (Status (MIDI (14 Note_on)))        │
    │ 9E   │ (Status (MIDI (15 Note_on)))        │
    │ 9F   │ (Status (MIDI (16 Note_on)))        │
    │ 80   │ (Status (MIDI (1 Note_off)))        │
    │ 81   │ (Status (MIDI (2 Note_off)))        │
    │ 82   │ (Status (MIDI (3 Note_off)))        │
    │ 83   │ (Status (MIDI (4 Note_off)))        │
    │ 84   │ (Status (MIDI (5 Note_off)))        │
    │ 85   │ (Status (MIDI (6 Note_off)))        │
    │ 86   │ (Status (MIDI (7 Note_off)))        │
    │ 87   │ (Status (MIDI (8 Note_off)))        │
    │ 88   │ (Status (MIDI (9 Note_off)))        │
    │ 89   │ (Status (MIDI (10 Note_off)))       │
    │ 8A   │ (Status (MIDI (11 Note_off)))       │
    │ 8B   │ (Status (MIDI (12 Note_off)))       │
    │ 8C   │ (Status (MIDI (13 Note_off)))       │
    │ 8D   │ (Status (MIDI (14 Note_off)))       │
    │ 8E   │ (Status (MIDI (15 Note_off)))       │
    │ 8F   │ (Status (MIDI (16 Note_off)))       │
    │ A0   │ (Status (MIDI (1 Aftertouch)))      │
    │ A1   │ (Status (MIDI (2 Aftertouch)))      │
    │ A2   │ (Status (MIDI (3 Aftertouch)))      │
    │ A3   │ (Status (MIDI (4 Aftertouch)))      │
    │ A4   │ (Status (MIDI (5 Aftertouch)))      │
    │ A5   │ (Status (MIDI (6 Aftertouch)))      │
    │ A6   │ (Status (MIDI (7 Aftertouch)))      │
    │ A7   │ (Status (MIDI (8 Aftertouch)))      │
    │ A8   │ (Status (MIDI (9 Aftertouch)))      │
    │ A9   │ (Status (MIDI (10 Aftertouch)))     │
    │ AA   │ (Status (MIDI (11 Aftertouch)))     │
    │ AB   │ (Status (MIDI (12 Aftertouch)))     │
    │ AC   │ (Status (MIDI (13 Aftertouch)))     │
    │ AD   │ (Status (MIDI (14 Aftertouch)))     │
    │ AE   │ (Status (MIDI (15 Aftertouch)))     │
    │ AF   │ (Status (MIDI (16 Aftertouch)))     │
    │ B0   │ (Status (MIDI (1 Controller)))      │
    │ B1   │ (Status (MIDI (2 Controller)))      │
    │ B2   │ (Status (MIDI (3 Controller)))      │
    │ B3   │ (Status (MIDI (4 Controller)))      │
    │ B4   │ (Status (MIDI (5 Controller)))      │
    │ B5   │ (Status (MIDI (6 Controller)))      │
    │ B6   │ (Status (MIDI (7 Controller)))      │
    │ B7   │ (Status (MIDI (8 Controller)))      │
    │ B8   │ (Status (MIDI (9 Controller)))      │
    │ B9   │ (Status (MIDI (10 Controller)))     │
    │ BA   │ (Status (MIDI (11 Controller)))     │
    │ BB   │ (Status (MIDI (12 Controller)))     │
    │ BC   │ (Status (MIDI (13 Controller)))     │
    │ BD   │ (Status (MIDI (14 Controller)))     │
    │ BE   │ (Status (MIDI (15 Controller)))     │
    │ BF   │ (Status (MIDI (16 Controller)))     │
    │ C0   │ (Status (MIDI (1 Program_change)))  │
    │ C1   │ (Status (MIDI (2 Program_change)))  │
    │ C2   │ (Status (MIDI (3 Program_change)))  │
    │ C3   │ (Status (MIDI (4 Program_change)))  │
    │ C4   │ (Status (MIDI (5 Program_change)))  │
    │ C5   │ (Status (MIDI (6 Program_change)))  │
    │ C6   │ (Status (MIDI (7 Program_change)))  │
    │ C7   │ (Status (MIDI (8 Program_change)))  │
    │ C8   │ (Status (MIDI (9 Program_change)))  │
    │ C9   │ (Status (MIDI (10 Program_change))) │
    │ CA   │ (Status (MIDI (11 Program_change))) │
    │ CB   │ (Status (MIDI (12 Program_change))) │
    │ CC   │ (Status (MIDI (13 Program_change))) │
    │ CD   │ (Status (MIDI (14 Program_change))) │
    │ CE   │ (Status (MIDI (15 Program_change))) │
    │ CF   │ (Status (MIDI (16 Program_change))) │
    │ D0   │ (Status (MIDI (1 Pressure)))        │
    │ D1   │ (Status (MIDI (2 Pressure)))        │
    │ D2   │ (Status (MIDI (3 Pressure)))        │
    │ D3   │ (Status (MIDI (4 Pressure)))        │
    │ D4   │ (Status (MIDI (5 Pressure)))        │
    │ D5   │ (Status (MIDI (6 Pressure)))        │
    │ D6   │ (Status (MIDI (7 Pressure)))        │
    │ D7   │ (Status (MIDI (8 Pressure)))        │
    │ D8   │ (Status (MIDI (9 Pressure)))        │
    │ D9   │ (Status (MIDI (10 Pressure)))       │
    │ DA   │ (Status (MIDI (11 Pressure)))       │
    │ DB   │ (Status (MIDI (12 Pressure)))       │
    │ DC   │ (Status (MIDI (13 Pressure)))       │
    │ DD   │ (Status (MIDI (14 Pressure)))       │
    │ DE   │ (Status (MIDI (15 Pressure)))       │
    │ DF   │ (Status (MIDI (16 Pressure)))       │
    │ E0   │ (Status (MIDI (1 Pitch_wheel)))     │
    │ E1   │ (Status (MIDI (2 Pitch_wheel)))     │
    │ E2   │ (Status (MIDI (3 Pitch_wheel)))     │
    │ E3   │ (Status (MIDI (4 Pitch_wheel)))     │
    │ E4   │ (Status (MIDI (5 Pitch_wheel)))     │
    │ E5   │ (Status (MIDI (6 Pitch_wheel)))     │
    │ E6   │ (Status (MIDI (7 Pitch_wheel)))     │
    │ E7   │ (Status (MIDI (8 Pitch_wheel)))     │
    │ E8   │ (Status (MIDI (9 Pitch_wheel)))     │
    │ E9   │ (Status (MIDI (10 Pitch_wheel)))    │
    │ EA   │ (Status (MIDI (11 Pitch_wheel)))    │
    │ EB   │ (Status (MIDI (12 Pitch_wheel)))    │
    │ EC   │ (Status (MIDI (13 Pitch_wheel)))    │
    │ ED   │ (Status (MIDI (14 Pitch_wheel)))    │
    │ EE   │ (Status (MIDI (15 Pitch_wheel)))    │
    │ EF   │ (Status (MIDI (16 Pitch_wheel)))    │
    │ F0   │ (Status Sysex)                      │
    │ F1   │ (Status Mtc_quarter_frame)          │
    │ F2   │ (Status Song_position)              │
    │ F3   │ (Status Song_select)                │
    │ F4   │ (Status U_F4)                       │
    │ F5   │ (Status U_F5)                       │
    │ F6   │ (Status Tune_request)               │
    │ F7   │ (Status End_sysex)                  │
    │ F8   │ (Status (Realtime Clock))           │
    │ F9   │ (Status (Realtime Tick))            │
    │ FA   │ (Status (Realtime Start))           │
    │ FB   │ (Status (Realtime Continue))        │
    │ FC   │ (Status (Realtime Stop))            │
    │ FD   │ (Status (Realtime FD))              │
    │ FE   │ (Status (Realtime Active_sense))    │
    │ FF   │ (Status (Realtime Reset))           │
    └──────┴─────────────────────────────────────┘
    |}]
;;
