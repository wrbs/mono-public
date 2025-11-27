open! Core
open Bonsai_term

module Line_type = struct
  type t =
    | Thin
    | Thick
    | Double
    | Round_corners

  let vbar = function
    | Thin | Round_corners -> "│"
    | Thick -> "┃"
    | Double -> "║"
  ;;

  let hbar = function
    | Thin | Round_corners -> "─"
    | Thick -> "━"
    | Double -> "═"
  ;;

  let tl = function
    | Thin -> "┌"
    | Thick -> "┏"
    | Double -> "╔"
    | Round_corners -> "╭"
  ;;

  let bl = function
    | Thin -> "└"
    | Thick -> "┗"
    | Double -> "╚"
    | Round_corners -> "╰"
  ;;

  let br = function
    | Thin -> "┘"
    | Thick -> "┛"
    | Double -> "╝"
    | Round_corners -> "╯"
  ;;

  let tr = function
    | Thin -> "┐"
    | Thick -> "┓"
    | Double -> "╗"
    | Round_corners -> "╮"
  ;;
end

let view
  ?(line_type = Line_type.Thin)
  ?(hide_left = false)
  ?(hide_right = false)
  ?(hide_top = false)
  ?(hide_bottom = false)
  ?(attrs = [])
  content
  =
  let get_corner_view hide_side_1 hide_side_2 char_if_shown =
    if hide_side_1 || hide_side_2 then View.none else View.text ~attrs char_if_shown
  in
  let { Dimensions.height; width } = View.dimensions content in
  let hbar = Line_type.hbar line_type in
  let vbar = Line_type.vbar line_type in
  let horizontal_border = List.create ~len:width (View.text ~attrs hbar) |> View.hcat in
  let vertical_border = List.create ~len:height (View.text ~attrs vbar) |> View.vcat in
  let top_border =
    if hide_top
    then View.none
    else
      View.hcat
        [ get_corner_view hide_left hide_top (Line_type.tl line_type)
        ; horizontal_border
        ; get_corner_view hide_right hide_top (Line_type.tr line_type)
        ]
  in
  let bottom_border =
    if hide_bottom
    then View.none
    else
      View.hcat
        [ get_corner_view hide_left hide_bottom (Line_type.bl line_type)
        ; horizontal_border
        ; get_corner_view hide_right hide_bottom (Line_type.br line_type)
        ]
  in
  let left_border = if hide_left then View.none else vertical_border in
  let right_border = if hide_right then View.none else vertical_border in
  View.vcat
    [ top_border; View.hcat [ left_border; content; right_border ]; bottom_border ]
;;
