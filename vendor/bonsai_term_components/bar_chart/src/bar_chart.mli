open! Core
open Bonsai_term

module Bar : sig
  type 'data t =
    { value : 'data
    ; label : string option
    ; color : Attr.Color.t option
    }
end

module Y_range : sig
  (** The min/max values on the y axis. [Use_most_extreme] will find the min / max value
      from the given data. *)
  type 'a t =
    | Constant of 'a
    | Use_most_extreme_value
end

val view
  :  (module Floatable.S with type t = 'data)
  -> ?theme:Theme.t (** Default: [Theme.catpuccin ~flavor:Mocha ~data_color:Blue] *)
  -> ?bar_padding:int
       (** Default [2]. Number of blank units on either side of each bar. *)
  -> ?bar_width:int (** Default [8]. Number of units each bar takes up horizontally. *)
  -> ?show_ylabels:bool (** Default [true]. *)
  -> ?show_xlabels:bool (** Default [true]. *)
  -> ?title:string option (** Default [None]. *)
  -> ?show_border:bool (** Default [true]. *)
  -> ?y_min:'data Y_range.t
       (** Default [Constant 0]. The minimum y value on the y axis. *)
  -> ?y_max:'data Y_range.t
       (** Default [Use_most_extreme_value]. The maximum y value on the y axis. *)
  -> max_bar_height:int (** The number of vertical cells the y axis will take up. *)
  -> 'data Bar.t list (** Each [Bar.t] in the list will be graphed from left to right. *)
  -> View.t
