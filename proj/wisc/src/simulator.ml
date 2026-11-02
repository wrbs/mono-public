open! Core
open! Hardcaml
open Ints

module IO = struct
  type _ t =
    | Read : U16.t -> U16.t t
    | Write : U16.t * U16.t -> unit t
    | Nope : unit t
end

type t =
  | T :
      { current_io : 'a IO.t
      ; step : 'a -> t @ once
      }
      -> t

let u16_of_bits bits = bits |> Bits.to_unsigned_int |> U16.of_int_exn
let bits_of_u16 u16 = u16 |> U16.to_int |> Bits.of_unsigned_int ~width:16

module Current_state = struct
  type t = Bits.t ref Cpu.O.t

  let pc (t : t) = u16_of_bits !(t.pc)
  let reg (t : t) reg = u16_of_bits !(Cpu.Registers.get t.registers reg)

  let sexp_of_t t =
    let pc = pc t in
    Sexp.List
      ([%sexp [ "pc"; (pc : U16.Hex.t) ]]
       :: List.map Register.all_of_real ~f:(fun r ->
         let name = [%to_string: Register.real] r in
         let u16 = reg t r in
         let i16 = I16.of_u16 u16 in
         let value = [%string "%{u16#U16.Hex}[%{i16#I16}]"] in
         [%sexp [ (name : string); (value : string) ]]))
  ;;
end

type create_result = Simulator of Current_state.t @@ many * t

let create ~init_pc : create_result @ once =
  let module Sim = Cyclesim.With_interface (Cpu.I) (Cpu.O) in
  let sim = Sim.create (Cpu.create (Scope.create ~flatten_design:true ())) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim ~clock_edge:Before in
  inputs.enable := Bits.vdd;
  inputs.init_pc := bits_of_u16 init_pc;
  let rec aux () : t @ once =
    Cyclesim.cycle sim;
    match Bits.to_bool !(outputs.io.enable) with
    | false -> T { current_io = Nope; step = (fun () -> aux ()) }
    | true ->
      let addr = u16_of_bits !(outputs.io.addr) in
      (match Bits.to_bool !(outputs.io.write) with
       | false ->
         T
           { current_io = Read addr
           ; step =
               (fun data ->
                 inputs.data_in := bits_of_u16 data;
                 aux ())
           }
       | true ->
         let data = u16_of_bits !(outputs.io.data) in
         T { current_io = Write (addr, data); step = (fun () -> aux ()) })
  in
  Simulator (Cyclesim.outputs sim ~clock_edge:After, aux ())
;;

let drive ~init_pc ~(f : 'a. 'a IO.t -> ('a, 'res) Continue_or_stop.t) =
  let rec aux (T { current_io; step }) =
    match f current_io with
    | Continue input -> aux (step input)
    | Stop res -> res
  in
  let (Simulator (_outputs, t)) = create ~init_pc in
  aux t
;;
