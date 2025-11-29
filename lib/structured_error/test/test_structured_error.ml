open! Core

let here = [%here]

module Structered_json = struct
  open Jsonaf.Export

  type t =
    { message : string
    ; data : Jsonaf.t option [@jsonaf.option]
    }
  [@@deriving jsonaf_of]
end

let%expect_test "Show some error types" =
  let show error =
    (* Sexp repr *)
    print_s [%sexp (error : Error.t)];
    (* Internal repr *)
    error
    |> Error.to_info
    |> Info.Internal_repr.of_info
    |> [%sexp_of: Info.Internal_repr.t]
    |> print_s;
    (* Structured repr *)
    let structured = error |> Structured_error.of_error in
    structured
    |> [%sexp_of: Structured_error.t]
    |> Expectree.sexp_to_string
    |> print_endline;
    (* Structured json *)
    structured |> Structured_error.to_json |> Jsonaf.to_string_hum |> print_endline;
    (* Structured message_and_json *)
    let message, data = Structured_error.to_message_and_data structured in
    { Structered_json.message; data }
    |> [%jsonaf_of: Structered_json.t]
    |> Jsonaf.to_string_hum
    |> print_endline
  in
  let string = Error.of_string "just string" in
  show string;
  [%expect
    {|
    "just string"
    (String "just string")
    Message
    ├─╴Message
    ├─╴message╶╴just string
    ╰─╴payload╶╴•
    "just string"
    {
      "message": "just string"
    }
    |}];
  let message_only = Error.create_s [%message "message-only sexp"] in
  show message_only;
  [%expect
    {|
    "message-only sexp"
    (Sexp "message-only sexp")
    Message
    ├─╴Message
    ├─╴message╶╴message-only sexp
    ╰─╴payload╶╴•
    "message-only sexp"
    {
      "message": "message-only sexp"
    }
    |}];
  let sexp_with_tags =
    Error.create_s [%message "with sexp tags" ~tag:"value" ~tag2:([ 1; 2; 3 ] : int list)]
  in
  show sexp_with_tags;
  [%expect
    {|
    ("with sexp tags" (tag value) (tag2 (1 2 3)))
    (Sexp ("with sexp tags" (tag value) (tag2 (1 2 3))))
    Message
    ├─╴Message
    ├─╴message╶╴with sexp tags
    ╰─╴payload
       ╰─╴Tags
          ├─╴tag╶╴value
          ╰─╴tag2
             ├─╴1
             ├─╴2
             ╰─╴3
    [
      "with sexp tags",
      {
        "tag": "value",
        "tag2": "(1 2 3)"
      }
    ]
    {
      "message": "with sexp tags",
      "data": {
        "tag": "value",
        "tag2": "(1 2 3)"
      }
    }
    |}];
  show (Error.of_list [ string; message_only; sexp_with_tags ]);
  [%expect
    {|
    ("just string" "message-only sexp"
     ("with sexp tags" (tag value) (tag2 (1 2 3))))
    (Of_list ()
     ((String "just string") (Sexp "message-only sexp")
      (Sexp ("with sexp tags" (tag value) (tag2 (1 2 3))))))
    Multi
    ├─╴Message
    │  ├─╴Message
    │  ├─╴message╶╴just string
    │  ╰─╴payload╶╴•
    ├─╴Message
    │  ├─╴Message
    │  ├─╴message╶╴message-only sexp
    │  ╰─╴payload╶╴•
    ╰─╴Message
       ├─╴Message
       ├─╴message╶╴with sexp tags
       ╰─╴payload
          ╰─╴Tags
             ├─╴tag╶╴value
             ╰─╴tag2
                ├─╴1
                ├─╴2
                ╰─╴3
    [
      "just string",
      "message-only sexp",
      [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    ]
    {
      "message": "multiple errors",
      "data": [
        "just string",
        "message-only sexp",
        [
          "with sexp tags",
          {
            "tag": "value",
            "tag2": "(1 2 3)"
          }
        ]
      ]
    }
    |}];
  show (string |> Error.tag ~tag:"thing");
  [%expect
    {|
    (thing "just string")
    (Tag_t thing (String "just string"))
    ╭─╴Wrapped
    ├──┬─╴Message
    │  ├─╴message╶╴thing
    │  ╰─╴payload╶╴•
    ╰─╴Message
       ├─╴Message
       ├─╴message╶╴just string
       ╰─╴payload╶╴•
    [
      "thing",
      "just string"
    ]
    {
      "message": "thing",
      "data": "just string"
    }
    |}];
  show (sexp_with_tags |> Error.tag ~tag:"thing");
  [%expect
    {|
    (thing ("with sexp tags" (tag value) (tag2 (1 2 3))))
    (Tag_t thing (Sexp ("with sexp tags" (tag value) (tag2 (1 2 3)))))
    ╭─╴Wrapped
    ├──┬─╴Message
    │  ├─╴message╶╴thing
    │  ╰─╴payload╶╴•
    ╰─╴Message
       ├─╴Message
       ├─╴message╶╴with sexp tags
       ╰─╴payload
          ╰─╴Tags
             ├─╴tag╶╴value
             ╰─╴tag2
                ├─╴1
                ├─╴2
                ╰─╴3
    [
      "thing",
      [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    ]
    {
      "message": "thing",
      "data": [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    }
    |}];
  show (sexp_with_tags |> Error.tag_s ~tag:[%message "Some tag"]);
  [%expect
    {|
    ("Some tag" ("with sexp tags" (tag value) (tag2 (1 2 3))))
    (Tag_arg "" "Some tag" (Sexp ("with sexp tags" (tag value) (tag2 (1 2 3)))))
    ╭─╴Wrapped
    ├──┬─╴Message
    │  ├─╴message╶╴Some tag
    │  ╰─╴payload╶╴•
    ╰─╴Message
       ├─╴Message
       ├─╴message╶╴with sexp tags
       ╰─╴payload
          ╰─╴Tags
             ├─╴tag╶╴value
             ╰─╴tag2
                ├─╴1
                ├─╴2
                ╰─╴3
    [
      "Some tag",
      [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    ]
    {
      "message": "Some tag",
      "data": [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    }
    |}];
  show (sexp_with_tags |> Error.tag_s ~tag:[%message "Some tag" ~param:"value"]);
  [%expect
    {|
    (("Some tag" (param value)) ("with sexp tags" (tag value) (tag2 (1 2 3))))
    (Tag_arg "" ("Some tag" (param value))
     (Sexp ("with sexp tags" (tag value) (tag2 (1 2 3)))))
    ╭─╴Wrapped
    ├──┬─╴Message
    │  ├─╴message╶╴Some tag
    │  ╰─╴payload
    │     ╰─╴Tags
    │        ╰─╴param╶╴value
    ╰─╴Message
       ├─╴Message
       ├─╴message╶╴with sexp tags
       ╰─╴payload
          ╰─╴Tags
             ├─╴tag╶╴value
             ╰─╴tag2
                ├─╴1
                ├─╴2
                ╰─╴3
    [
      "Some tag",
      {
        "param": "value"
      },
      [
        "with sexp tags",
        {
          "tag": "value",
          "tag2": "(1 2 3)"
        }
      ]
    ]
    {
      "message": "Some tag",
      "data": [
        {
          "param": "value"
        },
        [
          "with sexp tags",
          {
            "tag": "value",
            "tag2": "(1 2 3)"
          }
        ]
      ]
    }
    |}];
  show (Error.create "message" 3 [%sexp_of: int]);
  [%expect
    {|
    (message 3)
    (Tag_sexp message 3 ())
    Message
    ├─╴Message
    ├─╴message╶╴message
    ╰─╴payload
       ╰─╴Single╶╴3
    [
      "message",
      "3"
    ]
    {
      "message": "message",
      "data": "3"
    }
    |}];
  show (Error.create "message" 3 [%sexp_of: int] ~here);
  [%expect
    {|
    (message 3 lib/structured_error/test/test_structured_error.ml:3:11)
    (Tag_sexp message 3
     (lib/structured_error/test/test_structured_error.ml:3:11))
    Message
    ├─╴Message
    ├─╴message╶╴message
    ╰─╴payload
       ╰─╴Other
          ├─╴3
          ╰─╴lib/structured_error/test/test_structured_error.ml:3:11
    [
      "message",
      [
        "3",
        "lib/structured_error/test/test_structured_error.ml:3:11"
      ]
    ]
    {
      "message": "message",
      "data": [
        "3",
        "lib/structured_error/test/test_structured_error.ml:3:11"
      ]
    }
    |}];
  show (Error.create "name" 3 [%sexp_of: int] ~here:[%here]);
  [%expect
    {|
    (name 3 lib/structured_error/test/test_structured_error.ml:356:52)
    (Tag_sexp name 3 (lib/structured_error/test/test_structured_error.ml:356:52))
    Message
    ├─╴Message
    ├─╴message╶╴name
    ╰─╴payload
       ╰─╴Other
          ├─╴3
          ╰─╴lib/structured_error/test/test_structured_error.ml:356:52
    [
      "name",
      [
        "3",
        "lib/structured_error/test/test_structured_error.ml:356:52"
      ]
    ]
    {
      "message": "name",
      "data": [
        "3",
        "lib/structured_error/test/test_structured_error.ml:356:52"
      ]
    }
    |}];
  let exn = Exn.create_s [%message "Some exception" ~tag:"Value"] in
  show (Error.of_exn exn);
  [%expect
    {|
    ("Some exception" (tag Value))
    (Exn ("Some exception" (tag Value)))
    Message
    ├─╴Message
    ├─╴message╶╴Some exception
    ╰─╴payload
       ╰─╴Tags
          ╰─╴tag╶╴Value
    [
      "Some exception",
      {
        "tag": "Value"
      }
    ]
    {
      "message": "Some exception",
      "data": {
        "tag": "Value"
      }
    }
    |}];
  show (Error.of_exn Stdlib.Not_found);
  [%expect
    {|
    Not_found
    (Exn Not_found)
    Message
    ├─╴Message
    ├─╴message╶╴Not_found
    ╰─╴payload╶╴•
    "Not_found"
    {
      "message": "Not_found"
    }
    |}];
  let f () =
    show (Error.of_exn Stdlib.Not_found ~backtrace:(`This "some backtrace\nwith lines\n"));
    [%expect
      {|
      (Not_found ("some backtrace" "with lines"))
      (With_backtrace (Sexp Not_found)  "some backtrace\
                                       \nwith lines\
                                       \n")
      ╭─╴With_backtrace
      ├─╴Message
      │  ├─╴Message
      │  ├─╴message╶╴Not_found
      │  ╰─╴payload╶╴•
      ╰─╴some backtrace╶╴with lines
      [
        "Not_found",
        [
          "some backtrace",
          "with lines"
        ]
      ]
      {
        "message": "Not_found",
        "data": [
          "some backtrace",
          "with lines"
        ]
      }
      |}]
  in
  f ();
  ()
;;
