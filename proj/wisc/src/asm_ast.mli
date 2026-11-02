open! Core

module Register : sig
  type t

  include Identifiable.S [@mode local] with type t := t
end

module Label : sig
  module Name : sig
    type t

    include Identifiable.S [@mode local] with type t := t
  end

  module Dir : sig
    type t =
      | Forwards
      | Backwards
    [@@deriving sexp_of]
  end

  module Ref : sig
    type t =
      | Global of Name.t
      | Local of int * Dir.t
    [@@deriving sexp_of]
  end

  module Def : sig
    type t =
      | Global of Name.t
      | Local of int
    [@@deriving sexp_of]
  end
end

module Operand : sig
  type t =
    | Register of Register.t
    | Label of Label.Ref.t
    | Constant of int
  [@@deriving sexp_of]
end

module Opcode : sig
  type t

  include Identifiable.S with type t := t
end

module Parsed_instruction : sig
  type t =
    { opcode : Opcode.t
    ; operands : Operand.t list
    }
  [@@deriving sexp_of]
end

module Statement : sig
  type t =
    | Label_def of Label.Def.t
    | Instruction of Parsed_instruction.t
  [@@deriving sexp_of]
end

val parse : string -> Statement.t list

module For_testing : sig
  val print_shape : unit -> unit
end
