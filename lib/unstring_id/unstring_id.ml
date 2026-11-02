open! Core

module type S = sig
  include String_id.S_with_extras [@mode local]

  val unstring : t Unstring.t
end

let make ?quickcheck_generator unit_unstring ~module_name ~caller_identity =
  let module T = struct
    let unstring =
      let%map_open.Unstring s = capture unit_unstring in
      s
    ;;

    module Stable = struct
      module V1 = struct
        module T = struct
          type t = string
          [@@deriving
            compare ~localize, equal ~localize, globalize, hash, typerep, stable_witness]

          let of_string =
            unstage
              (Unstring.make_of_string
                 unstring
                 ~compile_when:`Lazily
                 ~on_ambiguity:`Pick_first)
          ;;

          let%template[@alloc _ = (stack, heap)] to_string s = s

          include functor Sexpable.Of_stringable [@alloc stack]

          let caller_identity = caller_identity

          include
            Binable.Stable.Of_binable.V2 [@mode local]
              (String)
              (struct
                type nonrec t = t

                let%template[@mode m = (global, local)] to_binable (t @ m) = t
                let of_binable = of_string
                let caller_identity = caller_identity
              end)

          include functor Comparator.Stable.V1.Make
        end

        include T
        include functor Comparable.Stable.V1.With_stable_witness.Make
        include functor Hashable.Stable.V1.With_stable_witness.Make
        include functor Diffable.Atomic.Make
      end
    end

    include Stable.V1.T
    include functor Comparable.Make_binable_using_comparator [@mode local]
    include functor Hashable.Make_binable
    include functor Diffable.Atomic.Make

    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
    let quickcheck_observer = String.quickcheck_observer

    let quickcheck_generator =
      Option.value_or_thunk quickcheck_generator ~default:(fun () ->
        String.gen_nonempty' Char.gen_print
        |> Quickcheck.Generator.filter ~f:(fun s ->
          match of_string s with
          | _ -> true
          | exception _ -> false))
    ;;

    include Pretty_printer.Register (struct
        type nonrec t = t

        let module_name = module_name
        let to_string = to_string
      end)

    let arg_type = Command.Arg_type.create of_string
  end
  in
  (module T : S)
;;
