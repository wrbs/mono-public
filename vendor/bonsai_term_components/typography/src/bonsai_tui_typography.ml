open! Core
module Text = Text

let typeset_line ~max_width line = Typesetting.typeset_line ~max_width line

let typeset ~max_width (text : 'attr Text.t list list) : 'attr Text.t list list =
  List.concat_map text ~f:(fun line ->
    match line with
    | [] -> [ [] ]
    | _ -> typeset_line ~max_width line)
;;

module For_testing = Typesetting.For_testing
