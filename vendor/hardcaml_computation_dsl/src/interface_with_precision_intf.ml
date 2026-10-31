(** Represents a Hardcaml interface with associated [Precision.t]s (representing Q-format
    precisions) for each field. *)
open Hardcaml

module type X = sig
  include Interface.S

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val precision : Precision.t t
end

module type Empty = X with type 'a t = 'a Hardcaml.Interface.Empty.t

module type With_valid_wrap = sig
  include With_valid.Wrap.S
  include X with type 'a t = 'a t
end

module type With_valid_wrap_include = sig
  type 'a value

  module With_valid : With_valid_wrap with type 'a value = 'a value
end

module type Interface_with_precision = sig
  module type S = X
  module type Empty = Empty

  (** A wrapper to make it easy to wrap [Interface_with_precision.S] in [With_valid]
      without having to re-define the [compare] and [precision] every time. *)
  module With_valid : sig
    module Wrap : sig
      module type S = With_valid_wrap

      module Make (X : X) : S

      module Include : sig
        module type S = With_valid_wrap_include
        module type F = functor (X : X) -> S with type 'a value := 'a X.t

        module Make (X : X) : S with type 'a value := 'a X.t
      end
    end
  end

  (** A wrapper around [Hardcaml.Interface.Empty]. *)
  module Empty : Empty
end
