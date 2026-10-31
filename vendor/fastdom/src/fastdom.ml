open! Core

(** The type of [q] guarantees that all reads and writes happen in the order that they are
    pushed to [q] *)
let q : (unit -> (unit -> unit)) Reversed_list.t ref = ref Reversed_list.[]

let read_then_write ~read ~write =
  let f () =
    let value = read () in
    fun () -> write value
  in
  q := f :: !q
;;

let read ~f = read_then_write ~read:f ~write:Fn.id
let write ~f = read_then_write ~read:Fn.id ~write:f
let run f = f ()

module Expert = struct
  let run_reads () =
    let writes =
      (* We aren't using [Reversed_list.rev_map] because that would execute the reads in
         the reverse order in which they were pushed to the list. While this shouldn't
         matter if the user didn't push a DOM mutation into a read, it is still
         technically possible that they did. In that case, running the methods in the
         reverse order may cause an unexpected interactions if the user was depending on
         the order of writes
      *)
      Reversed_list.rev !q |> List.map ~f:run
    in
    let ( (* Clear the queue *) ) = q := Reversed_list.[] in
    let run_writes () = List.iter writes ~f:run in
    `Run_writes run_writes
  ;;

  let run_actions () =
    let (`Run_writes run_writes) = run_reads () in
    run_writes ()
  ;;
end

module For_testing = struct
  let num_elements_in_queue () = Reversed_list.length !q
end
