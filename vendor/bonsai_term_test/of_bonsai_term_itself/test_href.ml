open! Core
open Bonsai_test
open Bonsai_term

let app ~dimensions:_ (local_ _graph) =
  Bonsai.return
    (View.hcat
       [ View.text "Visit "
       ; View.text
           ~attrs:
             [ Attr.href "https://example.com"
             ; Attr.fg (Attr.Color.rgb ~r:0 ~g:100 ~b:255)
             ; Attr.underline
             ]
           "example.com"
       ; View.text " for more info"
       ])
;;

let%expect_test "Not_ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Not_ansi
      ~initial_dimensions:{ width = 40; height = 1 }
      app
  in
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────┐
    │Visit example.com for more info         │
    └────────────────────────────────────────┘
    |}]
;;

let%expect_test "Ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Ansi
      ~initial_dimensions:{ width = 40; height = 1 }
      app
  in
  Handle.show handle;
  [%expect
    {| (off)Visit ]8;;https://example.com\(off fg:rgb256-0-100-255 +uline)example.com]8;;\(off) for more info(off)(EraseLine:ToEnd)(off) |}]
;;

(* Tests for edge cases with zcat and cropping *)

let test view =
  let app ~dimensions:_ (local_ _graph) = Bonsai.return view in
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Ansi
      ~initial_dimensions:{ width = 20; height = 1 }
      app
  in
  Handle.show handle
;;

let%expect_test "zcat of two href spans that exactly overlap" =
  test
    (View.zcat
       [ View.text ~attrs:[ Attr.href "https://first.com" ] "overlap"
       ; View.text ~attrs:[ Attr.href "https://second.com" ] "overlap"
       ]);
  [%expect {| ]8;;https://first.com\(off)overlap]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "zcat of two href spans where one span is inside the other" =
  test
    (View.zcat
       [ View.pad ~l:3 @@ View.text ~attrs:[ Attr.href "https://inner.com" ] "inner"
       ; View.text ~attrs:[ Attr.href "https://outer.com" ] "outer text here"
       ]);
  [%expect
    {| ]8;;https://outer.com\(off)out]8;;\]8;;https://inner.com\(off)inner]8;;\]8;;https://outer.com\(off)xt here]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "zcat of two href spans that share the same start position" =
  test
    (View.zcat
       [ View.text ~attrs:[ Attr.href "https://first.com" ] "first"
       ; View.text ~attrs:[ Attr.href "https://second.com" ] "second link"
       ]);
  [%expect
    {| ]8;;https://first.com\(off)first]8;;\]8;;https://second.com\(off)d link]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "zcat of two href spans that share the same end position" =
  test
    (View.zcat
       [ View.text ~attrs:[ Attr.href "https://first.com" ] "first link"
       ; View.hcat
           [ View.text "first "
           ; View.text ~attrs:[ Attr.href "https://second.com" ] "link"
           ]
       ]);
  [%expect
    {| ]8;;https://first.com\(off)first link]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "zcat of two href spans that partially overlap but are offset" =
  test
    (View.zcat
       [ View.text ~attrs:[ Attr.href "https://first.com" ] "first link"
       ; View.hcat
           [ View.text "fir"
           ; View.text ~attrs:[ Attr.href "https://second.com" ] "st link ext"
           ]
       ]);
  [%expect
    {| ]8;;https://first.com\(off)first link]8;;\]8;;https://second.com\(off) ext]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "cropping of nodes with the href attribute - crop from left" =
  test
    (View.crop
       ~l:5
       (View.text ~attrs:[ Attr.href "https://example.com" ] "example.com link"));
  [%expect
    {| ]8;;https://example.com\(off)le.com link]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "cropping of nodes with the href attribute - crop from right" =
  test
    (View.crop
       ~r:5
       (View.text ~attrs:[ Attr.href "https://example.com" ] "example.com link"));
  [%expect
    {| ]8;;https://example.com\(off)example.com]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "cropping of nodes with the href attribute - crop from both sides" =
  test
    (View.crop
       ~l:3
       ~r:5
       (View.text ~attrs:[ Attr.href "https://example.com" ] "example.com link"));
  [%expect
    {| ]8;;https://example.com\(off)mple.com]8;;\(off)(EraseLine:ToEnd)(off) |}]
;;

let%expect_test "cropping of nodes with the href attribute - crop to nothing" =
  test
    (View.crop ~l:20 (View.text ~attrs:[ Attr.href "https://example.com" ] "example.com"));
  [%expect {| (off)(EraseLine:ToEnd)(off) |}]
;;
