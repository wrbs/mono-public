open Core

module type%template [@mode m = (local, global)] S = sig
  (* control_chars not a char as there are a few * characters you will never get (e.g.) ^I
     = tab ^H = backspace ^M = '\r'
  *)
  type control_chars =
    [ `a
    | `b
    | `c
    | `d
    | `e
    | `f
    | `g
    | `j
    | `k
    | `l
    | `n
    | `o
    | `p
    | `q
    | `r
    | `s
    | `t
    | `u
    | `v
    | `w
    | `x
    | `y
    | `z
    ]
  [@@deriving sexp, bin_io, (compare [@mode.explicit m])]

  type basic_without_char =
    [ `Enter
    | `Down
    | `Up
    | `Left
    | `Right
    | `Page_up
    | `Page_down
    | `Home
    | `End
    | `F of int (* function key.  The int is in [0,64) *)
    | `Backspace
    | `Tab
    | `Shift_tab
    | `Shift_up
    | `Shift_down
    | `Shift_left
    | `Shift_right
    | `Esc
    | `Insert
    | `Delete
    | `Ctrl of control_chars
    | `Strange of int
    ]
  [@@deriving sexp, bin_io, (compare [@mode.explicit m])]

  type basic =
    [ `Char of char
    | basic_without_char
    ]
  [@@deriving bin_io, (compare [@mode.explicit m]), sexp]

  type t =
    [ basic
    | `Alt of basic
    ]
  [@@deriving sexp, bin_io, (compare [@mode.explicit m])]

  val to_string : t -> string
end

module M : S = struct
  type control_chars =
    [ `a
    | `b
    | `c
    | `d
    | `e
    | `f
    | `g
    | `j
    | `k
    | `l
    | `n
    | `o
    | `p
    | `q
    | `r
    | `s
    | `t
    | `u
    | `v
    | `w
    | `x
    | `y
    | `z
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  type basic_without_char =
    [ `Enter
    | `Down
    | `Up
    | `Left
    | `Right
    | `Page_up
    | `Page_down
    | `Home
    | `End
    | `F of int (* function key.  The int is in [0,64) *)
    | `Backspace
    | `Tab
    | `Shift_tab
    | `Shift_up
    | `Shift_down
    | `Shift_left
    | `Shift_right
    | `Esc
    | `Insert
    | `Delete
    | `Ctrl of control_chars
    | `Strange of int
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  type basic =
    [ `Char of char
    | basic_without_char
    ]
  [@@deriving bin_io, compare ~localize, sexp]

  type t =
    [ basic
    | `Alt of basic
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  let to_string_basic t =
    match t with
    | `Char ' ' -> "<Space>"
    | `Char c when Char.to_int c > 127 ->
      (* Encode latin1 characters (Codepoints 128 to 255) as Utf8. *)
      String.Utf8.init 1 ~f:(fun _ -> Uchar.of_char c) |> String.Utf8.to_string
    | `Char c -> String.of_char c
    | `Enter -> "<Enter>"
    | `Down -> "<Down>"
    | `Up -> "<Up>"
    | `Left -> "<Left>"
    | `Right -> "<Right>"
    | `Page_up -> "<PageUp>"
    | `Page_down -> "<PageDown>"
    | `Home -> "<Home>"
    | `End -> "<End>"
    | `F n -> Printf.sprintf "<F%i>" n
    | `Backspace -> "<Backspace>"
    | `Tab -> "<Tab>"
    | `Shift_tab -> "<Shift-Tab>"
    | `Shift_up -> "<Shift-Up>"
    | `Shift_down -> "<Shift-Down>"
    | `Shift_left -> "<Shift-Left>"
    | `Shift_right -> "<Shift-Right>"
    | `Esc -> "<Esc>"
    | `Insert -> "<Insert>"
    | `Delete -> "<Delete>"
    | `Strange k -> Printf.sprintf "<STRANGE%i>" k
    | `Ctrl (#control_chars as c) ->
      "<Ctrl>-" ^ (sexp_of_control_chars c |> Sexp.to_string)
  ;;

  let to_string t =
    match t with
    | #basic as t -> to_string_basic t
    | `Alt t -> "<Alt>-" ^ to_string_basic t
  ;;
end

include M

module%test [@name "to_string"] _ = struct
  let%expect_test "r" =
    print_string (to_string (`Char 'r'));
    [%expect {| r |}]
  ;;

  let%expect_test "R" =
    print_string (to_string (`Char 'R'));
    [%expect {| R |}]
  ;;

  let%expect_test "<Ctrl-r>" =
    print_string (to_string (`Ctrl `r));
    [%expect {| <Ctrl>-r |}]
  ;;

  let%expect_test "<Alt-r>" =
    print_string (to_string (`Alt (`Char 'r')));
    [%expect {| <Alt>-r |}]
  ;;

  let%expect_test "<Alt-R>" =
    print_string (to_string (`Alt (`Char 'R')));
    [%expect {| <Alt>-R |}]
  ;;

  let%expect_test "<Alt-Ctrl-r>" =
    print_string (to_string (`Alt (`Ctrl `r)));
    [%expect {| <Alt>-<Ctrl>-r |}]
  ;;

  let%expect_test "Pound sign" =
    print_string (to_string (`Char '\xA3'));
    [%expect {| £ |}]
  ;;
end
