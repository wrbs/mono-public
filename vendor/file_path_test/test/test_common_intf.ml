open! Core

module Definitions = struct
  module type Arg = sig @@ portable
    module Type : File_path.Type
    module Path : File_path.Common with module Type := Type
    module Examples : Examples.Common with type t := Type.t

    module Tested : sig
      val arg_type : Type.t Command.Arg_type.t
    end
  end

  module type S = sig
    module Tested : File_path.Common

    val run_expect_tests : unit -> unit
  end
end

module type Test_common = sig
  include module type of struct
    include Definitions
  end

  module Make (Arg : Arg) : S with module Tested.Type := Arg.Type
end
