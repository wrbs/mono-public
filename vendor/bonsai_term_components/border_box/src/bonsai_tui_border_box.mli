open! Core
open Bonsai_term

module Line_type : sig
  (* {v
                       ┌──────┐
      Thin:            │hello!│
                       └──────┘
                       ┏━━━━━━┓
      Thick:           ┃hello!┃
                       ┗━━━━━━┛
                       ╔══════╗
      Double:          ║hello!║
                       ╚══════╝
                       ╭──────╮
      Rounded_corners: │hello!│
                       ╰──────╯
     v} *)
  type t =
    | Thin
    | Thick
    | Double
    | Round_corners
end

(** [line_type] defaults to [Thin].

    Hiding a side removes the border on the respective side (and decreases the size of the
    returned [View.t] by 1 for that axis). Example of hiding the top and right sides:
    {v
    │hello!
    └──────
    v} *)
val view
  :  ?line_type:Line_type.t
  -> ?hide_left:bool
  -> ?hide_right:bool
  -> ?hide_top:bool
  -> ?hide_bottom:bool
  -> ?attrs:Attr.t list
  -> View.t
  -> View.t
