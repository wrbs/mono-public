open Base
open! Hardcaml

val generate
  :  ?custom_constraints:Rope.t
  -> ?dir:string
  -> name:string
  -> part:string
  -> pins:Pin.t list
  -> Board.t
  -> unit

val generate_files
  :  ?custom_constraints:Rope.t
  -> name:string
  -> pins:Pin.t list
  -> Board.t
  -> verilog:string * xdc:string

module For_testing : sig
  val rtl_of_hardcaml_circuit : Board.t -> String.t
end
