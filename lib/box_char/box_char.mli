open! Core

val horizontal : [ `thin | `thick | `double ] -> string
val vertical : [ `thin | `thick | `double ] -> string

val thin_thick
  :  ?left:[ `thin | `thick ]
  -> ?up:[ `thin | `thick ]
  -> ?right:[ `thin | `thick ]
  -> ?down:[ `thin | `thick ]
  -> ?curvy_thin_corner:unit
  -> unit
  -> string

val dashed
  :  dir:[ `horiz | `vert ]
  -> style:[ `four | `three | `two ]
  -> thickness:[ `thin | `thick ]
  -> string

val double_line : [ `horiz | `vert ] -> string

val double_junction
  :  horiz:[ `both | `left | `right ]
  -> vert:[ `both | `down | `up ]
  -> which_double:[ `both | `horiz | `vert ]
  -> string

val diagonal : [ `cross | `rising | `falling ] -> string
