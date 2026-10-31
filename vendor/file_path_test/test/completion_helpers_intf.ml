open! Core

module Definitions = struct
  module type Path = sig
    type t

    val of_string : string -> t
    val arg_type : t Command.Arg_type.t
  end
end

module type Completion_helpers = sig
  include module type of struct
    include Definitions
  end

  (** Tests autocompletion of file paths using the given arg type. *)
  val test_arg_type : (module Path with type t = 'a) -> unit
end
