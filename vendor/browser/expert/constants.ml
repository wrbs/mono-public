module Js = Js_of_ocaml.Js

module String = struct
  let string = Js.string "string"
  let boolean = Js.string "boolean"
  let number = Js.string "number"
  let constructor = Js.string "constructor"
  let object_ = Js.string "object"
  let function_ = Js.string "function"
end

module Number = struct
  let max_int = Int.max_int |> Int.to_float |> Js.float
  let min_int = Int.min_int |> Int.to_float |> Js.float
end

module Bool = struct
  let true_ = Js.bool true
  let false_ = Js.bool false
end
