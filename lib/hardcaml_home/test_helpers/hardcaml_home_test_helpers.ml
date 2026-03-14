open! Core
open! Hardcaml
module Waveform = Hardcaml_waveterm.Waveform

let hex_bits s = Bits.of_hex s ~width:(String.length s * 4)

module type Component = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Make_sim (C : Component) = struct
  module I = C.I
  module O = C.O
  module Sim = Cyclesim.With_interface (I) (O)

  let create ?config () =
    Sim.create (C.create (Scope.create ~flatten_design:true ())) ?config
  ;;

  let create_waveform () =
    Hardcaml_waveterm.Waveform.create (create ~config:Cyclesim.Config.trace_all ())
  ;;

  let inputs (sim : Sim.t) = Cyclesim.inputs sim
  let outputs (sim : Sim.t) ~edge = Cyclesim.outputs sim ~clock_edge:edge
  let io sim ~edge = inputs sim, outputs sim ~edge

  let update_inputs t f =
    let i = inputs t in
    let cur = I.map i ~f:( ! ) in
    let next_ = f cur in
    I.iter2 i next_ ~f:( := )
  ;;

  let testbench ?vcd ?waveform f =
    let sim, after =
      match waveform with
      | None ->
        ( create ?config:(Option.map vcd ~f:(fun _ -> Cyclesim.Config.trace_all)) ()
        , fun () -> () )
      | Some show ->
        let waves, sim = create_waveform () in
        sim, fun () -> show waves
    in
    let sim, after =
      match vcd with
      | None -> sim, after
      | Some filename ->
        let path =
          if String.is_prefix filename ~prefix:"~/"
          then Sys.getenv_exn "HOME" ^/ String.drop_prefix filename 2
          else filename
        in
        let oc = Out_channel.create path in
        let sim = Vcd.wrap oc sim in
        let after () =
          Out_channel.close oc;
          Stdio.print_endline ("Saved waves to " ^ filename);
          after ()
        in
        sim, after
    in
    (try f sim with
     | exn ->
       let bt = Backtrace.get () in
       print_s [%sexp [ "Testbench raised"; (exn : exn); (bt : Backtrace.t) ]]);
    after ()
  ;;
end
