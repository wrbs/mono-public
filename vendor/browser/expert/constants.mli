module Js = Js_of_ocaml.Js

module String : sig
  (** JavaScript string constants *)
  val string : Js.js_string Js.t

  val boolean : Js.js_string Js.t
  val number : Js.js_string Js.t
  val constructor : Js.js_string Js.t
  val object_ : Js.js_string Js.t
  val function_ : Js.js_string Js.t
end

module Number : sig
  (** JavaScript number constants *)

  val max_int : Js.number Js.t
  val min_int : Js.number Js.t
end

module Bool : sig
  val true_ : bool Js.t
  val false_ : bool Js.t
end
