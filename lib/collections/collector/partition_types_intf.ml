(** This is in its own module as they're a bit ugly and using them doesn't requiring
    knowing much more than "they're all vaguely lists" as documented in the main mli *)

module type S = sig
  type ('a, 'b) collector

  module Shape : sig
    type part = T

    type _ t =
      | [] : unit t
      | ( :: ) : part * 'rest t -> ('a -> 'rest) t
  end

  module Choice : sig
    type 'elems t
  end

  module Choices : sig
    type ('elems, 'choice) t' =
      | [] : (unit, 'choice) t'
      | ( :: ) :
          ('elem -> 'choice) * ('elems, 'choice) t'
          -> ('elem -> 'elems, 'choice) t'

    type 'elems t = ('elems, 'elems Choice.t) t'

    val of_shape : 'elems Shape.t -> 'elems t
    val shape : 'elems t -> 'elems Shape.t
  end

  module Collectors : sig
    type (_, _) t =
      | [] : (unit, unit) t
      | ( :: ) :
          ('elem, 'result) collector * ('elems, 'results) t
          -> ('elem -> 'elems, 'result -> 'results) t

    val elems_shape : ('elems, _) t -> 'elems Shape.t
    val results_shape : (_, 'results) t -> 'results Shape.t
    val choices : ('elems, _) t -> 'elems Choices.t
  end

  module Results : sig
    type _ t =
      | [] : unit t
      | ( :: ) : 'result * 'results t -> ('result -> 'results) t

    val shape : 'a t -> 'a Shape.t
  end
end
