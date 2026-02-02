open! Core
open! Hardcaml
open! Signal

type t =
  { write_ready : Signal.t
  ; read_ready : Signal.t
  ; read_data : Signal.t
  ; length : Signal.t
  ; is_full : Signal.t
  ; is_empty : Signal.t
  }

let create scope reg_spec ~capacity ~write_data ~write_valid ~reading =
  let length_bits = num_bits_to_represent capacity in
  let length = wire length_bits in
  let incr_ptr x =
    if capacity = 1 lsl length_bits
    then incr x
    else mux2 (x ==:. capacity) (zero length_bits) (incr x)
  in
  let%hw is_full = length ==:. capacity in
  let%hw is_empty = length ==:. 0 in
  let%hw write_ready = ~:is_full in
  let%hw read_ready = ~:is_empty in
  let%hw enq = write_ready &: write_valid in
  let%hw deq = reading &: read_ready in
  assign
    length
    (reg_fb reg_spec ~width:length_bits ~f:(fun prev ->
       mux (enq @: deq) [ prev; decr prev; incr prev; prev ]));
  let prev_read_ptr = wire length_bits in
  let read_ptr = mux2 deq (incr_ptr prev_read_ptr) prev_read_ptr in
  assign prev_read_ptr (reg reg_spec read_ptr);
  let%hw write_ptr =
    reg_fb reg_spec ~width:length_bits ~f:(fun ptr -> mux2 enq (incr_ptr ptr) ptr)
  in
  let read =
    Ram.create
      ~collision_mode:Write_before_read
      ~size:capacity
      ~write_ports:
        [| { write_clock = Reg_spec.clock reg_spec
           ; write_address = write_ptr
           ; write_data
           ; write_enable = enq
           }
        |]
      ~read_ports:
        [| { read_clock = Reg_spec.clock reg_spec
           ; read_address = read_ptr
           ; read_enable = vdd
           }
        |]
      ()
  in
  let%hw read_data = read.(0) in
  { write_ready; read_ready; read_data; length; is_full; is_empty }
;;

module Testing = struct
  module Make_sim (P : sig
      val bits : int
      val capacity : int
    end) =
  struct
    include P

    let length_bits = num_bits_to_represent (capacity - 1)

    module I = struct
      type 'a t =
        { write_data : 'a [@bits bits]
        ; write_valid : 'a
        ; reading : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { write_ready : 'a
        ; read_ready : 'a
        ; read_data : 'a [@bits bits]
        ; length : 'a [@bits length_bits]
        ; is_full : 'a
        ; is_empty : 'a
        }
      [@@deriving hardcaml]
    end

    let create () =
      let module Sim = Cyclesim.With_interface (I) (O) in
      Sim.create (fun i ->
        let%tydi { write_ready; read_ready; read_data; length; is_full; is_empty } =
          create
            (Scope.create ~flatten_design:true ())
            (Reg_spec.create ~clock:gnd ())
            ~capacity
            ~write_data:i.write_data
            ~write_valid:i.write_valid
            ~reading:i.reading
        in
        { write_ready; read_ready; read_data; length; is_full; is_empty })
    ;;
  end

  let test steps ~capacity =
    let bits =
      List.filter_map steps ~f:(function
        | Some to_push, _ -> Some to_push
        | _ -> None)
      |> List.max_elt ~compare:[%compare: int]
      |> Option.value_exn
      |> num_bits_to_represent
    in
    let module M =
      Make_sim (struct
        let bits = bits
        let capacity = capacity
      end)
    in
    let sim = M.create () in
    let waveform, sim = Hardcaml_waveterm.Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    List.iter steps ~f:(fun (enq, deq) ->
      inputs.reading := Bits.of_bool deq;
      let () =
        match enq with
        | None ->
          inputs.write_valid := Bits.gnd;
          inputs.write_data := Bits.zero bits
        | Some value ->
          inputs.write_valid := Bits.vdd;
          inputs.write_data := Bits.of_unsigned_int ~width:bits value
      in
      Cyclesim.cycle sim);
    Hardcaml_waveterm.Waveform.expect waveform ~wave_width:1
  ;;

  let%expect_test "Basic test" =
    test
      ~capacity:3
      [ None, false
      ; Some 3, false
      ; Some 5, true
      ; None, true
      ; Some 3, false
      ; Some 5, false
      ; Some 6, false
      ; Some 7, false
      ; None, true
      ; None, true
      ; None, true
      ; None, true
      ];
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │reading        ││        ┌───────┐               ┌───────────────   │
      │               ││────────┘       └───────────────┘                  │
      │               ││────┬───┬───┬───┬───┬───┬───┬───┬───────────────   │
      │write_data     ││ 0  │3  │5  │0  │3  │5  │6  │7  │0                 │
      │               ││────┴───┴───┴───┴───┴───┴───┴───┴───────────────   │
      │write_valid    ││    ┌───────┐   ┌───────────────┐                  │
      │               ││────┘       └───┘               └───────────────   │
      │is_empty       ││────────┐       ┌───┐                       ┌───   │
      │               ││        └───────┘   └───────────────────────┘      │
      │is_full        ││                            ┌───────┐              │
      │               ││────────────────────────────┘       └───────────   │
      │               ││────────┬───────┬───┬───┬───┬───────┬───┬───┬───   │
      │length         ││ 0      │1      │0  │1  │2  │3      │2  │1  │0     │
      │               ││────────┴───────┴───┴───┴───┴───────┴───┴───┴───   │
      │               ││────────┬───┬───┬───┬───────────────────┬───┬───   │
      │read_data      ││ 0      │3  │5  │0  │3                  │6  │5     │
      │               ││────────┴───┴───┴───┴───────────────────┴───┴───   │
      │read_ready     ││        ┌───────┐   ┌───────────────────────┐      │
      │               ││────────┘       └───┘                       └───   │
      │write_ready    ││────────────────────────────┐       ┌───────────   │
      │               ││                            └───────┘              │
      └───────────────┘└───────────────────────────────────────────────────┘
      069d1a1bc7ffd380b682054e495168bf
      |}]
  ;;
end
