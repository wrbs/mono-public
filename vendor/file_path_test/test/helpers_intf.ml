(** We codify several testing patterns here to make [File_path] testing rigorous. Every
    expect test in this library should call one of these helpers. *)

open! Core

module Definitions = struct
  (** An arbitrary input type used by a test. *)
  module type Type = sig
    type t [@@deriving compare, equal, globalize, quickcheck, sexp_of]

    include Invariant.S with type t := t
  end

  module type Fn = sig
    module Input : Type
    module Output : Type

    (** Used for functions that allocate an [exn] or an [Error.t] in the failure case. In
        all other cases of stack-allocated output, we can use [require_no_allocation]. *)
    val local_may_allocate : Output.t -> bool

    type t
    type local_t

    val apply : t -> Input.t -> Output.t
    val local_apply : local_t -> Input.t @ local -> Output.t @ local
  end

  module type Labelled = sig
    include Type

    type 'a fn
    type 'a local_fn

    val apply : 'a fn -> t -> 'a
    val local_apply : 'a local_fn -> t @ local -> 'a @ local
  end

  (** A path type (e.g. File_path.Part.t, or File_path.t). *)
  module type Path_type = sig
    type t [@@deriving globalize, quickcheck]

    include%template Identifiable.S [@alloc stack] [@mode local] with type t := t

    include Invariant.S with type t := t

    module Expert : sig
      val unchecked_of_canonical_string : string -> t
    end
  end

  (** One version of stable serializations for a path type, including containers. *)
  module type Version = sig
    type t [@@deriving hash]

    include Stable with type t := t
    include Hashable.Stable.V1.S with type key := t

    include
      Comparable.Stable.V1.S
      with type comparable := t
       and type comparator_witness := comparator_witness
  end
end

module type Helpers = sig
  include module type of struct
    include Definitions
  end

  (** Tests the given list of constants. Prints them and tests that they pass the path
      type's invariant. *)
  val test_constants : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests the type's comparison. Makes sure sorting a list is consistent regardless of
      input order. *)
  val test_compare : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests string conversions for a path type. Makes sure they round-trip, satisfy
      invariants, raise appropriately, and only allocate when necessary. *)
  val test_of_string : (module Path_type) -> string list -> unit

  (** Tests the invariant, unchecked "expert" construction, and consistency with
      [of_string]'s raising / canonicalizing behavior. *)
  val test_invariant : (module Path_type) -> string list -> unit

  (** Makes sure container types are constructed appropriately. (e.g., not just
      [module Map = String.Map]). *)
  val test_containers : (module Path_type with type t = 'a) -> 'a list -> unit

  (** Tests a boolean function. Prints [true] and [false] examples. Makes sure both cases
      are covered. Tests [correctness] for each input. *)
  val test_predicate
    :  examples:'input list
    -> correctness:('input -> bool -> unit)
    -> 'fn
    -> (module Fn with type t = 'fn and type Input.t = 'input and type Output.t = bool)
    -> unit

  (** Tests a generic function with the given input and output types. Tests [correctness]
      for each input. *)
  val test_function
    :  examples:'input list
    -> correctness:('input -> 'output -> unit)
    -> 'fn
    -> 'local_fn
    -> (module Fn
          with type t = 'fn
           and type local_t = 'local_fn
           and type Input.t = 'input
           and type Output.t = 'output)
    -> unit

  (** Like [test_function], when ['output] is immediate and thus crosses locality. Tests
      [correctness] for each input. *)
  val test_immediate
    : 'input ('output : value mod global).
    examples:'input list
    -> correctness:('input -> 'output -> unit)
    -> ('input @ local -> 'output)
    -> (module Fn
          with type t = 'input @ local -> 'output
           and type local_t = 'input @ local -> 'output @ local
           and type Input.t = 'input
           and type Output.t = 'output)
    -> unit

  (** All tests sharing a [Bin_shape_universe.t] must have unique bin_shape digests.

      Even if not referred to explicitly by clients, this is exported to clarify the
      behavior of [test_stable*] below. *)
  module Bin_shape_universe : sig
    type t

    (** Creates a fresh [t]. *)
    val create : unit -> t

    (** A single shared [t], allocated on demand. *)
    val default : t Lazy.t
  end

  (** Tests stable serializations and round-trips for the given examples, and uniqueness
      of bin-shape digests. *)
  val test_stable_version
    :  ?bin_shape_universe:Bin_shape_universe.t
         (** defaults to [force Bin_shape_universe.default] *)
    -> Source_code_position.t
    -> (module Version with type t = 'a)
    -> 'a list
    -> unit

  (** Tests stable serializations for containers with the given examples, and uniqueness
      of bin-shape digests (instantiated at [int] for polymorphic types). *)
  val test_stable_containers
    :  ?bin_shape_universe:Bin_shape_universe.t
         (** defaults to [force Bin_shape_universe.default] *)
    -> Source_code_position.t
    -> (module Version with type t = 'a)
    -> 'a list
    -> unit

  (** Functors for various function calling conventions (arity, exceptions, etc.) *)

  module Fn (Input : Type) (Output : Type) :
    Fn
    with type t = Input.t -> Output.t
     and type local_t = Input.t @ local -> Output.t @ local
     and type Input.t = Input.t
     and type Output.t = Output.t

  module Fn_local (Input : Type) (Output : Type) :
    Fn
    with type t = Input.t @ local -> Output.t
     and type local_t = Input.t @ local -> Output.t @ local
     and type Input.t = Input.t
     and type Output.t = Output.t

  module Fn2_local (A : Type) (B : Type) (Output : Type) :
    Fn
    with type t = A.t @ local -> B.t @ local -> Output.t
     and type local_t = A.t @ local -> B.t @ local -> Output.t @ local
     and type Input.t = A.t * B.t
     and type Output.t = Output.t

  module Fn_labelled (Input : Labelled) (Output : Type) :
    Fn
    with type t = Output.t Input.fn
     and type local_t = Output.t Input.local_fn
     and type Input.t = Input.t
     and type Output.t = Output.t

  module Fn_or_error (Input : Type) (Output : Type) :
    Fn
    with type t = Input.t -> Output.t Or_error.t
     and type local_t = Input.t @ local -> Output.t Or_error.t @ local
     and type Input.t = Input.t
     and type Output.t = Output.t Or_error.t

  module Fn_labelled_or_error (Input : Labelled) (Output : Type) :
    Fn
    with type t = Output.t Or_error.t Input.fn
     and type local_t = Output.t Or_error.t Input.local_fn
     and type Input.t = Input.t
     and type Output.t = Output.t Or_error.t

  module Fn_exn (Input : Type) (Output : Type) :
    Fn
    with type t = Input.t -> Output.t
     and type local_t = Input.t @ local -> Output.t @ local
     and type Input.t = Input.t
     and type Output.t = Output.t Or_error.t

  module Fn2_local_exn (A : Type) (B : Type) (Output : Type) :
    Fn
    with type t = A.t @ local -> B.t @ local -> Output.t
     and type local_t = A.t @ local -> B.t @ local -> Output.t @ local
     and type Input.t = A.t * B.t
     and type Output.t = Output.t Or_error.t

  module Fn_labelled_exn (Input : Labelled) (Output : Type) :
    Fn
    with type t = Output.t Input.fn
     and type local_t = Output.t Input.local_fn
     and type Input.t = Input.t
     and type Output.t = Output.t Or_error.t

  (** Functors wrapping the [Type] signature in polymorphic types. *)

  module Option_of (Type : Type) : Type with type t = Type.t option
  module List_of (Type : Type) : Type with type t = Type.t list
  module Nonempty_list_of (Type : Type) : Type with type t = Type.t Nonempty_list.t
  module Pair_of (A : Type) (B : Type) : Type with type t = A.t * B.t

  (** Functors wrapping [Examples] types. These tell [test_*] above how to apply a
      [With_*.t] as multiple arguments to a function. *)

  module With_if_under (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_if_under.t
     and type 'a fn = Type.t -> if_under:File_path.Absolute.t @ local -> 'a
     and type 'a local_fn =
      Type.t @ local -> if_under:File_path.Absolute.t @ local -> 'a @ local

  module With_prefix (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_prefix.t
     and type 'a fn = Type.t -> prefix:Type.t @ local -> 'a
     and type 'a local_fn = Type.t @ local -> prefix:Type.t @ local -> 'a @ local

  module With_prefix_local (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_prefix.t
     and type 'a fn = Type.t @ local -> prefix:Type.t @ local -> 'a
     and type 'a local_fn = Type.t @ local -> prefix:Type.t @ local -> 'a @ local

  module With_suffix (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_suffix.t
     and type 'a fn = Type.t -> suffix:File_path.Relative.t @ local -> 'a
     and type 'a local_fn =
      Type.t @ local -> suffix:File_path.Relative.t @ local -> 'a @ local

  module With_suffix_local (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_suffix.t
     and type 'a fn = Type.t @ local -> suffix:File_path.Relative.t @ local -> 'a
     and type 'a local_fn =
      Type.t @ local -> suffix:File_path.Relative.t @ local -> 'a @ local

  module With_under (Type : Type) :
    Labelled
    with type t = Type.t Examples.With_under.t
     and type 'a fn = Type.t -> under:File_path.Absolute.t @ local -> 'a
     and type 'a local_fn =
      Type.t @ local -> under:File_path.Absolute.t @ local -> 'a @ local
end
