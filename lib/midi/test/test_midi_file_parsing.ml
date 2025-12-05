open! Core
open! Async

let load_test name = Reader.file_contents [%string "test-files/test-%{name}.mid"]

let test s =
  let f = Midi_file.of_string s |> Or_error.ok_exn in
  print_s [%sexp (f : Midi_file.t)]
;;

let%expect_test _ =
  let%map file = load_test "c-major-scale" in
  test file;
  [%expect
    {|
    ((header ((format Single_track) (timing (Metrical 96))))
     (tracks
      ((((delta 0) (kind (Meta (Track_name "C Major Scale Test"))))
        ((delta 0) (kind (Meta (Copyright https://jazz-soft.net))))
        ((delta 0)
         (kind
          (Meta
           (Text
            "This is the most basic MIDI test to serve a template for more useful tests.\n"))))
        ((delta 0) (kind (Meta (Text "You must hear a C-Major scale."))))
        ((delta 0) (kind (Meta (Text " Now you must hear C5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 60) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 60) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear D5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 62) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 62) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear E5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 64) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 64) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear F5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 65) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 65) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear G5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 67) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 67) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear A5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 69) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 69) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear B5!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 71) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 71) (velocity 64))))))
        ((delta 0) (kind (Meta (Text " Now you must hear C6!"))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 72) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 72) (velocity 64))))))
        ((delta 0) (kind (Meta (Text "Thank you!"))))
        ((delta 0) (kind (Meta End_of_track)))))))
    |}]
;;

let%expect_test _ =
  let%map file = load_test "sysex" in
  test file;
  [%expect
    {|
    ((header ((format Single_track) (timing (Metrical 96))))
     (tracks
      ((((delta 0) (kind (Meta (Track_name "SysEx ID Request Test"))))
        ((delta 0) (kind (Meta (Copyright https://jazz-soft.net))))
        ((delta 0)
         (kind (Meta (Text "This test sends an Identity Request SysEx.\n"))))
        ((delta 0)
         (kind
          (Meta
           (Text "Your device may respond with an Identity Response SysEx."))))
        ((delta 0)
         (kind
          (Sysex
           ("00000000  7e 7f 06 01 f7                                    |~....|"))))
        ((delta 96) (kind (Meta (Text "Thank you!"))))
        ((delta 0) (kind (Meta End_of_track)))))))
    |}]
;;

let%expect_test _ =
  let%map file = load_test "2-tracks-type-0" in
  test file;
  [%expect {|
    ((header ((format Single_track) (timing (Metrical 96))))
     (tracks
      ((((delta 0)
         (kind (Meta (Track_name "Standard MIDI file type 0 (invalid)"))))
        ((delta 0) (kind (Meta (Copyright https://jazz-soft.net))))
        ((delta 0)
         (kind
          (Meta
           (Text
            "This file has two tracks instead of one. That makes it, technically, invalid.\n"))))
        ((delta 0) (kind (Meta (Text "Track 1"))))
        ((delta 96) (kind (MIDI (1 (Note_on (note 60) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 60) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 62) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 62) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 64) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 64) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 65) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 65) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 67) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 67) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 69) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 69) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 71) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 71) (velocity 64))))))
        ((delta 0) (kind (MIDI (1 (Note_on (note 72) (velocity 127))))))
        ((delta 96) (kind (MIDI (1 (Note_off (note 72) (velocity 64))))))
        ((delta 0) (kind (Meta End_of_track))))
       (((delta 0) (kind (Meta (Text "Track 2"))))
        ((delta 96) (kind (MIDI (2 (Note_on (note 61) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 61) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 63) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 63) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 65) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 65) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 66) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 66) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 68) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 68) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 70) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 70) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 72) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 72) (velocity 64))))))
        ((delta 0) (kind (MIDI (2 (Note_on (note 73) (velocity 127))))))
        ((delta 96) (kind (MIDI (2 (Note_off (note 73) (velocity 64))))))
        ((delta 0) (kind (Meta (Text "Thank you!"))))
        ((delta 0) (kind (Meta End_of_track)))))))
    |}]
;;
