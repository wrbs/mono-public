open! Core
open Ints

module Color = struct
  type t =
    { r : U4.t
    ; g : U4.t
    ; b : U4.t
    }
end

module Fg : sig
  type t = private U4.t

  include Ints.S with type t := t
end = struct
  include U4
end

module Bg : sig
  type t = private U4.t

  include Ints.S with type t := t
end = struct
  include U4
end

module Bank : sig
  type t = private U4.t

  include Ints.S with type t := t
end = struct
  include U4
end
