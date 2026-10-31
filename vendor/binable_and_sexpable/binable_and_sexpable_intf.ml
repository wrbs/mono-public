open Core

[%%template
[@@@mode.default m = (global, local)]

module type S = sig
  type t [@@deriving (bin_io [@mode m]), sexp]
end

module type Arg = sig
  module Format : S [@mode m]

  type t

  val to_format : t @ m -> Format.t @ m [@@mode m = (global, m)]
  val of_format : Format.t -> t
end

[@@@kind.default ka = (value, any)]

module type S1 = sig
  type ('a : ka) t [@@deriving (bin_io [@mode m]), sexp]
end

module type Arg1 = sig
  module Format : S1 [@kind ka]

  type ('a : ka) t

  val to_format : ('a : ka). 'a t @ m -> 'a Format.t @ m [@@mode m = (global, m)]
  val of_format : ('a : ka). 'a Format.t -> 'a t
end

[@@@kind.default kb = (value, any)]

module type S2 = sig
  type ('a : ka, 'b : kb) t [@@deriving (bin_io [@mode m]), sexp]
end

module type Arg2 = sig
  module Format : S2 [@kind ka kb]

  type ('a : ka, 'b : kb) t

  val to_format : ('a : ka) ('b : kb). ('a, 'b) t @ m -> ('a, 'b) Format.t @ m
  [@@mode m = (global, m)]

  val of_format : ('a : ka) ('b : kb). ('a, 'b) Format.t -> ('a, 'b) t
end]

module type%template Binable_and_sexpable = sig
  [@@@mode.default m = (global, local)]

  module type S = S [@mode m]
  module type Arg = Arg [@mode m]

  module%template.portable Of_format
      (Format : S
    [@mode m])
      (M : Arg [@mode m] with module Format := Format) : S [@mode m] with type t := M.t

  [@@@kind.default ka = (value, any)]

  module type S1 = S1 [@kind ka] [@mode m]
  module type Arg1 = Arg1 [@kind ka] [@mode m]

  module%template.portable Of_format1
      (Format : S1
    [@kind ka] [@mode m])
      (M : Arg1 [@kind ka] [@mode m] with module Format := Format) :
    S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a M.t

  [@@@kind.default kb = (value, any)]

  module type S2 = S2 [@kind ka kb] [@mode m]
  module type Arg2 = Arg2 [@kind ka kb] [@mode m]

  module%template.portable Of_format2
      (Format : S2
    [@kind ka kb] [@mode m])
      (M : Arg2 [@kind ka kb] [@mode m] with module Format := Format) :
    S2 [@kind ka kb] [@mode m] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t
end
