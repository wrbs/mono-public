open! Core

module Timing = struct
  module Base = struct
    type t =
      { blocks : int
      ; secs : float
      ; beats : float
      }
  end

  module Duration = struct
    type t =
      | Samples of float
      | Secs of float
      | Beats of float
  end

  module Instant = struct
    type t =
      { base : Base.t
      ; offset : Duration.t
      }
  end

  module Wait_result = struct end
end
