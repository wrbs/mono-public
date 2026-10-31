open Core
open Hardcaml

module Divide_by_power =
  Hardcaml_computation_dsl_standard_functions.Hardcaml.Divide_by_power

include struct
  open Divide_by_power
  module Inclusive_integer_range = Inclusive_integer_range
end

let debug = false

module Make (Config : Divide_by_power.Config) = struct
  module Divide_by_power = Divide_by_power.Make (Config)
  module Sim = Cyclesim.With_interface (Divide_by_power.I) (Divide_by_power.O)

  module Step =
    Hardcaml_step_testbench.Functional.Cyclesim.Make
      (Divide_by_power.I)
      (Divide_by_power.O)

  module Output = struct
    let shift =
      match Config.signedness with
      | Signed -> Config.output_i_bits - 1
      | Unsigned -> Config.output_i_bits
    ;;

    let min =
      match Config.signedness with
      | Signed -> Bigint.(-(one lsl shift))
      | Unsigned -> Bigint.zero
    ;;

    let max = Bigint.((one lsl shift) - one)

    type t =
      { quotient : Bigint.t
      ; division_output_not_multiple : bool
      ; error : bool
      }
    [@@deriving sexp_of, compare ~localize]
  end

  let mul_by_pow value ~pow:pow_base =
    if pow_base > 0
    then Bigint.(value * pow (of_int Config.base) (of_int pow_base))
    else Bigint.(value / pow (of_int Config.base) (-of_int pow_base))
  ;;

  let div_by_pow value ~pow:pow_base =
    if pow_base > 0
    then Bigint.(value / pow (of_int Config.base) (of_int pow_base))
    else Bigint.(value * pow (of_int Config.base) (-of_int pow_base))
  ;;

  let is_multiple_of value ~pow:pow_base =
    if pow_base < 0
    then failwith "only call [is_multiple_of] when you're actually dividing"
    else Bigint.(value % pow (of_int Config.base) (of_int pow_base) = zero)
  ;;

  module Input = struct
    let shift =
      match Config.signedness with
      | Signed -> Config.input_i_bits - 1
      | Unsigned -> Config.input_i_bits
    ;;

    let min =
      match Config.signedness with
      | Signed -> Bigint.(-(one lsl shift))
      | Unsigned -> Bigint.zero
    ;;

    let max = Bigint.((one lsl shift) - one)
    let min_before_overflow ~pow = mul_by_pow Output.min ~pow
    let max_before_overflow ~pow = mul_by_pow Output.max ~pow

    type t =
      { dividend : Bigint.t
      ; pow : int
      }
    [@@deriving sexp_of, compare ~localize]
  end

  (* Generate several negative (out of range), negative (in range), zero, positive (in
     range), and positive (out of range) input values. *)
  let gen_systematic ~pow =
    let of_quotient quotient = { Input.dividend = mul_by_pow ~pow quotient; pow } in
    let of_dividend dividend = { Input.dividend; pow } in
    let open Quickcheck.Generator.Let_syntax in
    let%bind negative_in_range =
      match Config.signedness with
      | Signed -> Bigint.gen_incl Output.min Bigint.minus_one |> List.gen_with_length 5
      | Unsigned -> return []
    in
    let%bind positive_in_range =
      Bigint.gen_incl Bigint.zero Output.max |> List.gen_with_length 5
    in
    let plus bigint offset = Bigint.(bigint + of_int offset) in
    let multiples =
      let in_range_boundary =
        [ Output.min
        ; plus Output.min 1
        ; plus Output.min 2
        ; Bigint.zero
        ; plus Output.max (-1)
        ; Output.max
        ]
      in
      (if Config.check_for_error
       then
         (match Config.signedness with
          | Signed ->
            [ div_by_pow ~pow Input.min
            ; plus (div_by_pow ~pow Input.min) 1
            ; plus (div_by_pow ~pow Input.min) 10000
            ; plus Output.min (-10000)
            ; plus Output.min (-1000)
            ; plus Output.min (-1)
            ]
          | Unsigned -> [])
         @ in_range_boundary
         @ [ plus Output.max 1
           ; plus Output.max 10000
           ; plus (div_by_pow ~pow Input.max) (-10000)
           ; plus (div_by_pow ~pow Input.max) (-1)
           ; div_by_pow ~pow Input.max
           ]
       else in_range_boundary)
      @ negative_in_range
      @ positive_in_range
    in
    let not_multiples =
      if Config.check_for_error
      then
        (match Config.signedness with
         | Signed ->
           [ plus (Input.min_before_overflow ~pow) (-3)
           ; plus (Input.min_before_overflow ~pow) (-2)
           ; plus (Input.min_before_overflow ~pow) (-1)
           ; Bigint.of_int (-25)
           ; Bigint.of_int (-3)
           ; Bigint.of_int (-2)
           ; Bigint.of_int (-1)
           ]
         | Unsigned -> [])
        @ [ Input.min_before_overflow ~pow
          ; plus (Input.min_before_overflow ~pow) 1
          ; plus (Input.min_before_overflow ~pow) 2
          ; Bigint.of_int 1
          ; Bigint.of_int 2
          ; Bigint.of_int 3
          ; Bigint.of_int 25
          ; plus (Input.max_before_overflow ~pow) (-3)
          ; plus (Input.max_before_overflow ~pow) (-2)
          ; plus (Input.max_before_overflow ~pow) (-1)
          ; Input.max_before_overflow ~pow
          ; plus (Input.max_before_overflow ~pow) 1
          ; plus (Input.max_before_overflow ~pow) 2
          ]
      else []
    in
    return (List.map ~f:of_quotient multiples @ List.map ~f:of_dividend not_multiples)
  ;;

  let gen_random ~pow:pow_base =
    let open Quickcheck.Generator.Let_syntax in
    let gen min max =
      Bigint.gen_incl min max
      >>| (if pow_base <= 0 || Config.check_for_error
           then Fn.id
           else (
             let to_multiple_of = Bigint.(pow (of_int Config.base) (of_int pow_base)) in
             fun v ->
               if Bigint.(v > zero)
               then Bigint.round_down v ~to_multiple_of
               else Bigint.round_up v ~to_multiple_of))
      >>| fun dividend -> { Input.dividend; pow = pow_base }
    in
    let gen_valid_range =
      gen
        (Input.min_before_overflow ~pow:pow_base)
        (Input.max_before_overflow ~pow:pow_base)
    in
    let gen_full_range = gen Input.min Input.max in
    let%bind full_input_range =
      if Config.check_for_error
      then List.gen_with_length 100 gen_full_range
      else return []
    in
    let%bind valid_input_range = List.gen_with_length 200 gen_valid_range in
    return (full_input_range @ valid_input_range)
  ;;

  let generate_inputs ~systematic =
    let open Quickcheck.Generator.Let_syntax in
    let rec loop res pow =
      if pow = Config.pow_base_bounds.max + 1
      then return (List.concat res)
      else (
        let%bind next = if systematic then gen_systematic ~pow else gen_random ~pow in
        loop (next :: res) (pow + 1))
    in
    loop [] Config.pow_base_bounds.min
  ;;

  let validate ~verbose ~(inputs : Input.t list) ~(outputs : Output.t list) =
    List.iter2_exn inputs outputs ~f:(fun ({ dividend; pow } as input) output ->
      if verbose then print_s [%message (input : Input.t) (output : Output.t)];
      let quotient = div_by_pow dividend ~pow in
      (* We check in a couple of levels. First, if the input overflows, just check that
         the module correctly catches that. Second, if the input did not overflow, then
         check whether the dividend was in fact a multiple of the divisor - if so, check
         that this is properly handled. Otherwise, check all the outputs. *)
      let min_input_before_overflow = Input.min_before_overflow ~pow in
      let max_input_before_overflow = Input.max_before_overflow ~pow in
      if Bigint.(
           dividend < min_input_before_overflow || dividend > max_input_before_overflow)
      then
        if Config.check_for_error
        then [%test_result: bool] output.error ~expect:true
        else
          raise_s
            [%message
              "Invalid input - this module assumes a separate circuit will be \
               instantiated to check whether the dividend was out of bounds!"
                (input : Input.t)]
      else if pow > 0 && not (is_multiple_of dividend ~pow)
      then
        if Config.check_for_error
        then (
          [%test_result: bool] output.error ~expect:true;
          [%test_result: bool] output.division_output_not_multiple ~expect:true)
        else
          raise_s
            [%message
              "Invalid input - this module assumes a separate circuit will be \
               instantiated to check whether the dividend is a perfect integral multiple \
               of the divisor."
                (input : Input.t)]
      else
        [%test_result: Output.t]
          output
          ~expect:{ quotient; error = false; division_output_not_multiple = false })
  ;;

  let send_inputs_and_capture_outputs h inputs =
    let outputs = Queue.create () in
    let captures =
      List.map inputs ~f:(fun { Input.dividend; pow } ->
        ignore
          (Step.cycle
             h
             { Step.input_hold with
               valid = Bits.vdd
             ; dividend =
                 Bits.of_bigint_trunc
                   ~width:Divide_by_power.I.port_widths.dividend
                   dividend
             ; pow = Bits.of_signed_int ~width:Divide_by_power.I.port_widths.pow pow
             }
           : Step.O_data.t);
        Step.spawn h (fun ev _ ->
          let o =
            Step.cycle ev ~num_cycles:(Divide_by_power.latency - 1) Step.input_hold
          in
          [%test_result: Bits.t] o.after_edge.valid ~expect:Bits.vdd;
          Queue.enqueue
            outputs
            { Output.quotient =
                Bits.to_bigint ~signedness:Config.signedness o.after_edge.quotient
            ; division_output_not_multiple =
                Bits.to_bool o.after_edge.division_output_not_multiple
            ; error = Bits.to_bool o.after_edge.error
            }))
    in
    ignore (Step.cycle h { Step.input_hold with valid = Bits.gnd } : Step.O_data.t);
    List.iter captures ~f:(Step.wait_for h);
    Queue.to_list outputs
  ;;

  let testbench inputs h _ =
    (* Send the inputs and capture the outputs. *)
    let outputs = send_inputs_and_capture_outputs h inputs in
    outputs
  ;;

  let create_sim () =
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (Divide_by_power.hierarchical
           ~build_mode:Simulation
           (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
    in
    let waves, sim = Cyclesim.Waveform.create_if ~enabled:debug sim in
    if debug
    then
      at_exit (fun () ->
        Hardcaml_waveterm.Waveform.Serialize.marshall
          (Option.value_exn waves)
          [%string "debug.hardcamlwaveform"]);
    sim
  ;;

  let test ?(verbose = false) inputs =
    let simulator = create_sim () in
    let testbench = testbench inputs in
    let outputs = Step.run_until_finished () ~simulator ~testbench in
    validate ~verbose ~inputs ~outputs
  ;;
end

module Config = struct
  let base = 5
  let pow_base_bounds = Inclusive_integer_range.create ~min:(-3) ~max:9
  let input_i_bits = 64
  let output_i_bits = 27
  let rom_latency = 1
  let multiply_latency = 3
  let signedness = Signedness.Signed
end

module%test [@name "Default Config, with [check_for_error = false]"] _ = struct
  open Make (struct
      include Config

      let check_for_error = false
    end)

  let%expect_test "basic test" =
    let inputs =
      Quickcheck.Generator.generate
        (gen_systematic ~pow:2)
        ~size:1
        ~random:(Splittable_random.of_int 0)
    in
    test ~verbose:true inputs;
    [%expect
      {|
      ((input ((dividend -1677721600) (pow 2)))
       (output
        ((quotient -67108864) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721575) (pow 2)))
       (output
        ((quotient -67108863) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721550) (pow 2)))
       (output
        ((quotient -67108862) (division_output_not_multiple false) (error false))))
      ((input ((dividend 0) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721550) (pow 2)))
       (output
        ((quotient 67108862) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721575) (pow 2)))
       (output
        ((quotient 67108863) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1661607950) (pow 2)))
       (output
        ((quotient -66464318) (division_output_not_multiple false) (error false))))
      ((input ((dividend -640058450) (pow 2)))
       (output
        ((quotient -25602338) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1141028675) (pow 2)))
       (output
        ((quotient -45641147) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1658850075) (pow 2)))
       (output
        ((quotient -66354003) (division_output_not_multiple false) (error false))))
      ((input ((dividend -371200050) (pow 2)))
       (output
        ((quotient -14848002) (division_output_not_multiple false) (error false))))
      ((input ((dividend 77347400) (pow 2)))
       (output
        ((quotient 3093896) (division_output_not_multiple false) (error false))))
      ((input ((dividend 678166750) (pow 2)))
       (output
        ((quotient 27126670) (division_output_not_multiple false) (error false))))
      ((input ((dividend 708189875) (pow 2)))
       (output
        ((quotient 28327595) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1493693725) (pow 2)))
       (output
        ((quotient 59747749) (division_output_not_multiple false) (error false))))
      ((input ((dividend 935321525) (pow 2)))
       (output
        ((quotient 37412861) (division_output_not_multiple false) (error false))))
      |}]
  ;;

  let%expect_test "comprehensive systematic test" =
    Quickcheck.test ~trials:5 (generate_inputs ~systematic:true) ~f:test
  ;;

  let%expect_test "randomized quickchecks " =
    Quickcheck.test ~trials:30 (generate_inputs ~systematic:false) ~f:test
  ;;
end

module%test [@name "Default Config, with [check_for_error = true]"] _ = struct
  open Make (struct
      include Config

      let check_for_error = true
    end)

  let%expect_test "basic test" =
    let inputs =
      Quickcheck.Generator.generate
        (gen_systematic ~pow:2)
        ~size:1
        ~random:(Splittable_random.of_int 0)
    in
    test ~verbose:true inputs;
    [%expect
      {|
      ((input ((dividend -9223372036854775800) (pow 2)))
       (output
        ((quotient -59055800) (division_output_not_multiple true) (error true))))
      ((input ((dividend -9223372036854775775) (pow 2)))
       (output
        ((quotient -59055799) (division_output_not_multiple true) (error true))))
      ((input ((dividend -9223372036854525800) (pow 2)))
       (output
        ((quotient -59045800) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677971600) (pow 2)))
       (output
        ((quotient 67098864) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677746600) (pow 2)))
       (output
        ((quotient 67107864) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721625) (pow 2)))
       (output
        ((quotient 67108863) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721600) (pow 2)))
       (output
        ((quotient -67108864) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721575) (pow 2)))
       (output
        ((quotient -67108863) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721550) (pow 2)))
       (output
        ((quotient -67108862) (division_output_not_multiple false) (error false))))
      ((input ((dividend 0) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721550) (pow 2)))
       (output
        ((quotient 67108862) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721575) (pow 2)))
       (output
        ((quotient 67108863) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721600) (pow 2)))
       (output
        ((quotient -67108864) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1677971575) (pow 2)))
       (output
        ((quotient -67098865) (division_output_not_multiple true) (error true))))
      ((input ((dividend 9223372036854525800) (pow 2)))
       (output
        ((quotient 59045800) (division_output_not_multiple true) (error true))))
      ((input ((dividend 9223372036854775775) (pow 2)))
       (output
        ((quotient 59055799) (division_output_not_multiple true) (error true))))
      ((input ((dividend 9223372036854775800) (pow 2)))
       (output
        ((quotient 59055800) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1661607950) (pow 2)))
       (output
        ((quotient -66464318) (division_output_not_multiple false) (error false))))
      ((input ((dividend -640058450) (pow 2)))
       (output
        ((quotient -25602338) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1141028675) (pow 2)))
       (output
        ((quotient -45641147) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1658850075) (pow 2)))
       (output
        ((quotient -66354003) (division_output_not_multiple false) (error false))))
      ((input ((dividend -371200050) (pow 2)))
       (output
        ((quotient -14848002) (division_output_not_multiple false) (error false))))
      ((input ((dividend 77347400) (pow 2)))
       (output
        ((quotient 3093896) (division_output_not_multiple false) (error false))))
      ((input ((dividend 678166750) (pow 2)))
       (output
        ((quotient 27126670) (division_output_not_multiple false) (error false))))
      ((input ((dividend 708189875) (pow 2)))
       (output
        ((quotient 28327595) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1493693725) (pow 2)))
       (output
        ((quotient 59747749) (division_output_not_multiple false) (error false))))
      ((input ((dividend 935321525) (pow 2)))
       (output
        ((quotient 37412861) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721603) (pow 2)))
       (output
        ((quotient -61740155) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721602) (pow 2)))
       (output
        ((quotient -18790482) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721601) (pow 2)))
       (output
        ((quotient 24159191) (division_output_not_multiple true) (error true))))
      ((input ((dividend -25) (pow 2)))
       (output ((quotient -1) (division_output_not_multiple false) (error false))))
      ((input ((dividend -3) (pow 2)))
       (output
        ((quotient 5368709) (division_output_not_multiple true) (error true))))
      ((input ((dividend -2) (pow 2)))
       (output
        ((quotient 48318382) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1) (pow 2)))
       (output
        ((quotient -42949673) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721600) (pow 2)))
       (output
        ((quotient -67108864) (division_output_not_multiple false) (error false))))
      ((input ((dividend -1677721599) (pow 2)))
       (output
        ((quotient -24159191) (division_output_not_multiple true) (error true))))
      ((input ((dividend -1677721598) (pow 2)))
       (output
        ((quotient 18790482) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1) (pow 2)))
       (output
        ((quotient 42949673) (division_output_not_multiple true) (error true))))
      ((input ((dividend 2) (pow 2)))
       (output
        ((quotient -48318382) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3) (pow 2)))
       (output
        ((quotient -5368709) (division_output_not_multiple true) (error true))))
      ((input ((dividend 25) (pow 2)))
       (output ((quotient 1) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721572) (pow 2)))
       (output
        ((quotient -61740156) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1677721573) (pow 2)))
       (output
        ((quotient -18790483) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1677721574) (pow 2)))
       (output
        ((quotient 24159190) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1677721575) (pow 2)))
       (output
        ((quotient 67108863) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1677721576) (pow 2)))
       (output
        ((quotient -24159192) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1677721577) (pow 2)))
       (output
        ((quotient 18790481) (division_output_not_multiple true) (error true))))
      |}]
  ;;

  let%expect_test "comprehensive systematic test" =
    Quickcheck.test ~trials:1 (generate_inputs ~systematic:true) ~f:test
  ;;

  let%expect_test "randomized quickchecks " =
    Quickcheck.test ~trials:30 (generate_inputs ~systematic:false) ~f:test
  ;;
end

module%test [@name "Default Config, with [signedness = Unsigned]"] _ = struct
  open Make (struct
      include Config

      let signedness = Signedness.Unsigned
      let check_for_error = true
    end)

  (* Note: We can't add tests that show (error true) but (division_output_not_multiple
     false), because the two errors are waterfalled. ie. the value of
     [division_output_not_multiple] is only valid if [division_input_too_big] is false. *)
  let%expect_test "basic test" =
    let inputs =
      Quickcheck.Generator.generate
        (gen_systematic ~pow:2)
        ~size:1
        ~random:(Splittable_random.of_int 0)
    in
    test ~verbose:true inputs;
    [%expect
      {|
      ((input ((dividend 0) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple false) (error false))))
      ((input ((dividend 25) (pow 2)))
       (output ((quotient 1) (division_output_not_multiple false) (error false))))
      ((input ((dividend 50) (pow 2)))
       (output ((quotient 2) (division_output_not_multiple false) (error false))))
      ((input ((dividend 0) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple false) (error false))))
      ((input ((dividend 3355443150) (pow 2)))
       (output
        ((quotient 134217726) (division_output_not_multiple false) (error false))))
      ((input ((dividend 3355443175) (pow 2)))
       (output
        ((quotient 134217727) (division_output_not_multiple false) (error false))))
      ((input ((dividend 3355443200) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3355693175) (pow 2)))
       (output ((quotient 9999) (division_output_not_multiple true) (error true))))
      ((input ((dividend 18446744073709301600) (pow 2)))
       (output
        ((quotient 118101600) (division_output_not_multiple true) (error true))))
      ((input ((dividend 18446744073709551575) (pow 2)))
       (output
        ((quotient 118111599) (division_output_not_multiple true) (error true))))
      ((input ((dividend 18446744073709551600) (pow 2)))
       (output
        ((quotient 118111600) (division_output_not_multiple true) (error true))))
      ((input ((dividend 16113650) (pow 2)))
       (output
        ((quotient 644546) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1037663150) (pow 2)))
       (output
        ((quotient 41506526) (division_output_not_multiple false) (error false))))
      ((input ((dividend 536692925) (pow 2)))
       (output
        ((quotient 21467717) (division_output_not_multiple false) (error false))))
      ((input ((dividend 18871525) (pow 2)))
       (output
        ((quotient 754861) (division_output_not_multiple false) (error false))))
      ((input ((dividend 2984243150) (pow 2)))
       (output
        ((quotient 119369726) (division_output_not_multiple false) (error false))))
      ((input ((dividend 0) (pow 2)))
       (output ((quotient 0) (division_output_not_multiple false) (error false))))
      ((input ((dividend 1) (pow 2)))
       (output
        ((quotient 42949673) (division_output_not_multiple true) (error true))))
      ((input ((dividend 2) (pow 2)))
       (output
        ((quotient 85899346) (division_output_not_multiple true) (error true))))
      ((input ((dividend 1) (pow 2)))
       (output
        ((quotient 42949673) (division_output_not_multiple true) (error true))))
      ((input ((dividend 2) (pow 2)))
       (output
        ((quotient 85899346) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3) (pow 2)))
       (output
        ((quotient 128849019) (division_output_not_multiple true) (error true))))
      ((input ((dividend 25) (pow 2)))
       (output ((quotient 1) (division_output_not_multiple false) (error false))))
      ((input ((dividend 3355443172) (pow 2)))
       (output
        ((quotient 5368708) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3355443173) (pow 2)))
       (output
        ((quotient 48318381) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3355443174) (pow 2)))
       (output
        ((quotient 91268054) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3355443175) (pow 2)))
       (output
        ((quotient 134217727) (division_output_not_multiple false) (error false))))
      ((input ((dividend 3355443176) (pow 2)))
       (output
        ((quotient 42949672) (division_output_not_multiple true) (error true))))
      ((input ((dividend 3355443177) (pow 2)))
       (output
        ((quotient 85899345) (division_output_not_multiple true) (error true))))
      |}]
  ;;

  let%expect_test "comprehensive systematic test" =
    Quickcheck.test ~trials:1 (generate_inputs ~systematic:true) ~f:test
  ;;

  let%expect_test "randomized quickchecks " =
    Quickcheck.test ~trials:30 (generate_inputs ~systematic:false) ~f:test
  ;;
end
