open Core
open Hardcaml
open Hardcaml_lws

module Dut = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; x : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { y : 'a } [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    Assertions.add scope "x_is_high" i.x;
    { O.y = i.x }
  ;;
end

module L = Lws_cyclesim.With_interface (Dut.I) (Dut.O)

type sim_context = Lws_context.M(Dut.I)(Dut.O).t

let create () = L.create ~config:{ Lws.Config.default with assertions = true } Dut.create
let print_assertions sim = print_s [%sexp (Lws_cyclesim.assertions sim : Assertions.t)]

let%expect_test "assertion holds for all cycles" =
  let sim = create () in
  Lws_cyclesim.run sim (fun (h @ local) ({ inputs; _ } : sim_context) ->
    inputs.x := Bits.vdd;
    Lws.step ~n:5 h);
  print_assertions sim;
  [%expect {| (((x_is_high Not_violated))) |}]
;;

let%expect_test "assertion is violated on cycles when x is low" =
  let sim = create () in
  Lws_cyclesim.run sim (fun (h @ local) ({ inputs; _ } : sim_context) ->
    inputs.x := Bits.vdd;
    Lws.step h;
    inputs.x := Bits.gnd;
    Lws.step ~n:2 h;
    inputs.x := Bits.vdd;
    Lws.step h;
    inputs.x := Bits.gnd;
    Lws.step h);
  print_assertions sim;
  [%expect {| (((x_is_high (Violated (1 2 4 5))))) |}]
;;
