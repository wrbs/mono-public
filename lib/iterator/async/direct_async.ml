open! Core
open! Async

type _ effect = Await : 'a Deferred.t -> 'a aliased many effect

module E = Effect.Make (struct
    type 'a t = 'a effect
  end)

type async = E.t
type t = async Effect.Handler.t

let await t deferred = (E.perform t (Await deferred)).many.aliased

let rec handle_result (result @ unique) =
  match result with
  | E.Value x -> return x
  | Exception exn -> Stdlib.raise exn
  | Operation (Await deferred, cont) ->
    let%bind wait_result = Monitor.try_with (fun () -> deferred) in
    let cont @ unique = Obj.magic_unique cont in
    handle_result
      (match wait_result with
       | Ok value -> Effect.continue cont { many = { aliased = value } } []
       | Error exn -> Effect.discontinue cont exn [])
;;

let run f = handle_result (E.run (fun async -> f ~async [@nontail]))
