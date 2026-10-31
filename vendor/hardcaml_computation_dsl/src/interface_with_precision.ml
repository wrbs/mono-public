include Interface_with_precision_intf

module type S = X

module With_valid = struct
  module Wrap = struct
    module type S = With_valid_wrap

    module Make (X : X) = struct
      include Hardcaml.With_valid.Wrap.Make (X)

      let compare f (a : _ t) (b : _ t) =
        let t = f a.valid b.valid in
        if t = 0 then X.compare f a.value b.value else t
      ;;

      let (precision : _ t) = { valid = Precision.bool; value = X.precision }
    end

    module Include = struct
      module type S = With_valid_wrap_include
      module type F = functor (X : X) -> S with type 'a value := 'a X.t

      module Make (X : X) = struct
        module With_valid = Make (X)
      end
    end
  end
end

module Empty = struct
  include Hardcaml.Interface.Empty

  let compare _ _ _ = 0
  let precision = Empty
end
