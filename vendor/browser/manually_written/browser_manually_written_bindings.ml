module Unsafe = struct
  module Error = struct
    type t = Not_in_correct_context

    (* The below methods are required to be exposed for convenience when users are trying
       to use this type in conjunction with ppx derivers or functors in [Core].
       Unfortunately this means that we have to write things manually due to explicitly
       wanting to avoid a dependency on base/core in the browser bindings. Thankfully,
       these methods are trivial to verify correctness of *)

    let sexp_of_t = function
      | Not_in_correct_context -> Sexplib.Sexp.Atom "Not_in_correct_context"
    ;;

    let t_of_sexp = function
      | Sexplib.Sexp.Atom "Not_in_correct_context" -> Not_in_correct_context
      | _ -> assert false
    ;;

    let compare a b =
      match a, b with
      | Not_in_correct_context, Not_in_correct_context -> 0
    ;;

    let equal a b =
      match a, b with
      | Not_in_correct_context, Not_in_correct_context -> true
    ;;
  end

  type 'a t = ('a, Error.t) result
end

module Context = struct
  type 'a t =
    | Secure : [ `Secure ] t
    | Cross_origin_isolated : [ `Cross_origin_isolated | `Secure ] t
end

module Unsafe_context_checker = struct
  module type S_input = sig
    type global

    val is_in_context : 'a. context:'a Context.t -> global -> bool
    val get_global : unit -> global
  end

  type t = { f : 'a. context:'a Context.t -> bool }

  let make (module S : S_input) =
    { f = (fun ~context -> S.is_in_context ~context (S.get_global ())) }
  ;;

  let check ~context t = t.f ~context
end
