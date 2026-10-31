open! Core
open Hardcaml

include Static.Make (struct
    type shared_interfaces = unit

    let process_shared ~valid_pipeline:_ ~interfaces_by_cycle_number =
      if not (Hashtbl.is_empty interfaces_by_cycle_number)
      then raise_s [%message "fully-pipelined cannot have shared operators"]
    ;;
  end)

(* Repackage the shared interface to get rid of the extra [unit] field. *)

type t =
  { latency : int
  ; overflow_cnt : Signal.t
  ; outputs : Signal.t list
  ; output_valid : Signal.t
  ; output_overflow : Signal.t
  }

let create
  ?only_clear_counters
  ~build_mode
  scope
  ~clocking
  ~start
  ~input_signal_by_node
  ~computation_graph
  =
  let { latency
      ; overflow_cnt
      ; outputs
      ; output_valid
      ; output_overflow
      ; shared_interfaces = _
      }
    =
    create
      ?only_clear_counters
      ~build_mode
      scope
      ~clocking
      ~start
      ~input_signal_by_node
      ~computation_graph
  in
  { latency; overflow_cnt; outputs; output_valid; output_overflow }
;;
