open! Core

module type Examples = sig
  type t

  val canonical : t
  val canonical_string : string
  val non_canonical_string : string
  val least : t
end

module Make
    (Type : File_path.Type)
    (Path : File_path.Common with module Type := Type)
    (Examples : Examples with type t := Path.t) :
  File_path.Common with module Type := Type = struct
  type t = Path.t
  [@@deriving
    compare ~localize
    , equal ~localize
    , globalize
    , quickcheck
    , sexp ~stackify
    , sexp_grammar]

  let arg_type = Path.arg_type

  [%%template
  include (
    Path :
    sig
      include
        Identifiable.S
        [@alloc stack] [@mode local portable]
        with type t := t
         and type comparator_witness = Path.comparator_witness
    end)

  let[@alloc stack] of_string = (Path.of_string [@alloc stack])

  module [@alloc a @ l = (heap_global, stack_local)] _ = struct
    let%bench_fun "equal =" =
      let x = Sys.opaque_identity Examples.canonical in
      let y = Sys.opaque_identity Examples.canonical in
      fun () -> (equal [@mode l]) x y
    ;;

    let%bench_fun "equal <>" =
      let x = Sys.opaque_identity Examples.least in
      let y = Sys.opaque_identity Examples.canonical in
      fun () -> (equal [@mode l]) x y
    ;;

    let%bench_fun "compare =" =
      let x = Sys.opaque_identity Examples.least in
      let y = Sys.opaque_identity Examples.least in
      fun () -> (compare [@mode l]) x y
    ;;

    let%bench_fun "compare <" =
      let x = Sys.opaque_identity Examples.least in
      let y = Sys.opaque_identity Examples.canonical in
      fun () -> (compare [@mode l]) x y
    ;;

    let%bench_fun "compare >" =
      let x = Sys.opaque_identity Examples.canonical in
      let y = Sys.opaque_identity Examples.least in
      fun () -> (compare [@mode l]) x y
    ;;

    let%bench_fun "of_string, canonical" =
      let string = Sys.opaque_identity Examples.canonical_string in
      fun () -> ignore ((of_string [@alloc a]) string : t)
    ;;

    let%bench_fun "of_string, non-canonical" =
      let string = Sys.opaque_identity Examples.non_canonical_string in
      fun () -> ignore ((of_string [@alloc a]) string : t)
    ;;

    let%bench_fun "to_string" =
      let t = Sys.opaque_identity Examples.canonical in
      fun () -> ignore ((to_string [@alloc a]) t : string)
    ;;

    let%bench_fun "sexp_of_t" =
      let t = Sys.opaque_identity Examples.canonical in
      fun () -> ignore ((sexp_of_t [@alloc a]) t : Sexp.t)
    ;;
  end

  let%bench_fun "t_of_sexp, canonical" =
    let string = Sys.opaque_identity (Sexp.Atom Examples.canonical_string) in
    fun () -> t_of_sexp string
  ;;

  let%bench_fun "t_of_sexp, non-canonical" =
    let string = Sys.opaque_identity (Sexp.Atom Examples.non_canonical_string) in
    fun () -> t_of_sexp string
  ;;

  let invariant = Path.invariant

  let%bench_fun "invariant" =
    let t = Sys.opaque_identity Examples.canonical in
    fun () -> invariant t
  ;;

  module Expert = struct
    let unchecked_of_canonical_string =
      (Path.Expert.unchecked_of_canonical_string [@alloc a])
    [@@alloc a = (stack, heap)]
    ;;

    module [@alloc a = (heap, stack)] _ = struct
      let%bench_fun "unchecked_of_canonical_string" =
        let string = Sys.opaque_identity Examples.canonical_string in
        fun () -> ignore ((unchecked_of_canonical_string [@alloc a]) string : t)
      ;;
    end
  end]
end

module%bench Part =
  Make (File_path.Types.Part) (File_path.Part)
    (struct
      let canonical_string = "foo"

      (** all valid strings are canonical, we just choose a slightly longer string *)
      let non_canonical_string = ".foo"

      let canonical = File_path.Part.of_string canonical_string
      let least = File_path.Part.dot
    end)

module Part = File_path.Part

module%bench Absolute =
  Make (File_path.Types.Absolute) (File_path.Absolute)
    (struct
      let canonical_string = "/foo/bar/baz"
      let non_canonical_string = "/foo//bar/baz/"
      let canonical = File_path.Absolute.of_string canonical_string
      let least = File_path.Absolute.root
    end)

module Absolute = File_path.Absolute

module%bench Relative =
  Make (File_path.Types.Relative) (File_path.Relative)
    (struct
      let canonical_string = "foo/bar/baz"
      let non_canonical_string = "foo//bar/baz/"
      let canonical = File_path.Relative.of_string canonical_string
      let least = File_path.Relative.dot
    end)

module Relative = File_path.Relative

module%bench Path =
  Make (File_path.Types.Path) (File_path)
    (struct
      let canonical_string = "/foo/bar/baz"
      let non_canonical_string = "/foo//bar/baz/"
      let canonical = File_path.of_string canonical_string
      let least = File_path.root
    end)

module Path = File_path
