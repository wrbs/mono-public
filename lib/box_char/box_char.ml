open! Core

let thin_thick
  ?(left : [ `thin | `thick ] option)
  ?(up : [ `thin | `thick ] option)
  ?(right : [ `thin | `thick ] option)
  ?(down : [ `thin | `thick ] option)
  ?(curvy_thin_corner : unit option)
  ()
  =
  match left, up, right, down with
  | None, None, None, None -> " "
  | Some `thin, None, Some `thin, None -> "─"
  | Some `thick, None, Some `thick, None -> "━"
  | None, Some `thin, None, Some `thin -> "│"
  | None, Some `thick, None, Some `thick -> "┃"
  | None, None, Some `thin, Some `thin ->
    if Option.is_some curvy_thin_corner then "╭" else "┌"
  | None, None, Some `thick, Some `thin -> "┍"
  | None, None, Some `thin, Some `thick -> "┎"
  | None, None, Some `thick, Some `thick -> "┏"
  | Some `thin, None, None, Some `thin ->
    if Option.is_some curvy_thin_corner then "╮" else "┐"
  | Some `thick, None, None, Some `thin -> "┑"
  | Some `thin, None, None, Some `thick -> "┒"
  | Some `thick, None, None, Some `thick -> "┓"
  | None, Some `thin, Some `thin, None ->
    if Option.is_some curvy_thin_corner then "╰" else "└"
  | None, Some `thin, Some `thick, None -> "┕"
  | None, Some `thick, Some `thin, None -> "┖"
  | None, Some `thick, Some `thick, None -> "┗"
  | Some `thin, Some `thin, None, None ->
    if Option.is_some curvy_thin_corner then "╯" else "┘"
  | Some `thick, Some `thin, None, None -> "┙"
  | Some `thin, Some `thick, None, None -> "┚"
  | Some `thick, Some `thick, None, None -> "┛"
  | None, Some `thin, Some `thin, Some `thin -> "├"
  | None, Some `thin, Some `thick, Some `thin -> "┝"
  | None, Some `thick, Some `thin, Some `thin -> "┞"
  | None, Some `thin, Some `thin, Some `thick -> "┟"
  | None, Some `thick, Some `thin, Some `thick -> "┠"
  | None, Some `thick, Some `thick, Some `thin -> "┡"
  | None, Some `thin, Some `thick, Some `thick -> "┢"
  | None, Some `thick, Some `thick, Some `thick -> "┣"
  | Some `thin, Some `thin, None, Some `thin -> "┤"
  | Some `thick, Some `thin, None, Some `thin -> "┥"
  | Some `thin, Some `thick, None, Some `thin -> "┦"
  | Some `thin, Some `thin, None, Some `thick -> "┧"
  | Some `thin, Some `thick, None, Some `thick -> "┨"
  | Some `thick, Some `thick, None, Some `thin -> "┩"
  | Some `thick, Some `thin, None, Some `thick -> "┪"
  | Some `thick, Some `thick, None, Some `thick -> "┫"
  | Some `thin, None, Some `thin, Some `thin -> "┬"
  | Some `thick, None, Some `thin, Some `thin -> "┭"
  | Some `thin, None, Some `thick, Some `thin -> "┮"
  | Some `thick, None, Some `thick, Some `thin -> "┯"
  | Some `thin, None, Some `thin, Some `thick -> "┰"
  | Some `thick, None, Some `thin, Some `thick -> "┱"
  | Some `thin, None, Some `thick, Some `thick -> "┲"
  | Some `thick, None, Some `thick, Some `thick -> "┳"
  | Some `thin, Some `thin, Some `thin, None -> "┴"
  | Some `thick, Some `thin, Some `thin, None -> "┵"
  | Some `thin, Some `thin, Some `thick, None -> "┶"
  | Some `thick, Some `thin, Some `thick, None -> "┷"
  | Some `thin, Some `thick, Some `thin, None -> "┸"
  | Some `thick, Some `thick, Some `thin, None -> "┹"
  | Some `thin, Some `thick, Some `thick, None -> "┺"
  | Some `thick, Some `thick, Some `thick, None -> "┻"
  | Some `thin, Some `thin, Some `thin, Some `thin -> "┼"
  | Some `thick, Some `thin, Some `thin, Some `thin -> "┽"
  | Some `thin, Some `thin, Some `thick, Some `thin -> "┾"
  | Some `thick, Some `thin, Some `thick, Some `thin -> "┿"
  | Some `thin, Some `thick, Some `thin, Some `thin -> "╀"
  | Some `thin, Some `thin, Some `thin, Some `thick -> "╁"
  | Some `thin, Some `thick, Some `thin, Some `thick -> "╂"
  | Some `thick, Some `thick, Some `thin, Some `thin -> "╃"
  | Some `thin, Some `thick, Some `thick, Some `thin -> "╄"
  | Some `thick, Some `thin, Some `thin, Some `thick -> "╅"
  | Some `thin, Some `thin, Some `thick, Some `thick -> "╆"
  | Some `thick, Some `thick, Some `thick, Some `thin -> "╇"
  | Some `thick, Some `thin, Some `thick, Some `thick -> "╈"
  | Some `thick, Some `thick, Some `thin, Some `thick -> "╉"
  | Some `thin, Some `thick, Some `thick, Some `thick -> "╊"
  | Some `thick, Some `thick, Some `thick, Some `thick -> "╋"
  | Some `thin, None, None, None -> "╴"
  | None, Some `thin, None, None -> "╵"
  | None, None, Some `thin, None -> "╶"
  | None, None, None, Some `thin -> "╷"
  | Some `thick, None, None, None -> "╸"
  | None, Some `thick, None, None -> "╹"
  | None, None, Some `thick, None -> "╺"
  | None, None, None, Some `thick -> "╻"
  | Some `thin, None, Some `thick, None -> "╼"
  | None, Some `thin, None, Some `thick -> "╽"
  | Some `thick, None, Some `thin, None -> "╾"
  | None, Some `thick, None, Some `thin -> "╿ "
;;

let dashed
  ~(dir : [ `horiz | `vert ])
  ~(style : [ `two | `three | `four ])
  ~(thickness : [ `thin | `thick ])
  =
  match dir, style, thickness with
  | `horiz, `two, `thin -> "╌"
  | `horiz, `two, `thick -> "╍"
  | `horiz, `three, `thin -> "┄"
  | `horiz, `three, `thick -> "┅"
  | `horiz, `four, `thin -> "┈"
  | `horiz, `four, `thick -> "┉"
  | `vert, `two, `thin -> "╎"
  | `vert, `two, `thick -> "╏"
  | `vert, `three, `thin -> "┆"
  | `vert, `three, `thick -> "┇"
  | `vert, `four, `thin -> "┊"
  | `vert, `four, `thick -> "┋"
;;

let double_line = function
  | `horiz -> "═"
  | `vert -> "║"
;;

let double_junction
  ~(horiz : [ `both | `left | `right ])
  ~(vert : [ `both | `down | `up ])
  ~(which_double : [ `both | `horiz | `vert ])
  =
  match horiz, vert, which_double with
  | `right, `down, `horiz -> "╒"
  | `right, `down, `vert -> "╓"
  | `right, `down, `both -> "╔"
  | `left, `down, `horiz -> "╕"
  | `left, `down, `vert -> "╖"
  | `left, `down, `both -> "╗"
  | `right, `up, `horiz -> "╘"
  | `right, `up, `vert -> "╙"
  | `right, `up, `both -> "╚"
  | `left, `up, `horiz -> "╛"
  | `left, `up, `vert -> "╜"
  | `left, `up, `both -> "╝"
  | `right, `both, `horiz -> "╞"
  | `right, `both, `vert -> "╟"
  | `right, `both, `both -> "╠"
  | `left, `both, `horiz -> "╡"
  | `left, `both, `vert -> "╢"
  | `left, `both, `both -> "╣"
  | `both, `down, `horiz -> "╤"
  | `both, `down, `vert -> "╥"
  | `both, `down, `both -> "╦"
  | `both, `up, `horiz -> "╧"
  | `both, `up, `vert -> "╨"
  | `both, `up, `both -> "╩"
  | `both, `both, `horiz -> "╪"
  | `both, `both, `vert -> "╫"
  | `both, `both, `both -> "╬"
;;

let diagonal = function
  | `cross -> "╳"
  | `rising -> "╱"
  | `falling -> "╲"
;;

let horizontal = function
  | (`thin | `thick) as style -> thin_thick ~left:style ~right:style ()
  | `double -> double_line `horiz
;;

let vertical = function
  | (`thin | `thick) as style -> thin_thick ~up:style ~down:style ()
  | `double -> double_line `vert
;;
