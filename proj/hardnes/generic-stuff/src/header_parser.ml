open! Core
open! Hardcaml
open! Signal

module Fields_and_bits = struct
  type t =
    { fields : int
    ; data : int
    }
  [@@deriving sexp_of]

  let zero = { fields = 0; data = 0 }
  let add t t' = { fields = t.fields + t'.fields; data = t.data + t'.data }
  let add_field t ~bits = { fields = t.fields + 1; data = t.data + bits }
end

module Step = struct
  type t =
    { ready : Fields_and_bits.t
    ; leftover : int (* how many bits need to go into the next buffer *)
    ; prev : Fields_and_bits.t
    }
  [@@deriving sexp_of]
end

let get_steps widths ~stream_bits =
  let rec aux
    ~steps_rev
    ~unused_bits
    ~remaining
    ~(ready : Fields_and_bits.t)
    ~(prev : Fields_and_bits.t)
    =
    match remaining with
    | [] ->
      (match unused_bits with
       | 0 -> List.rev ({ Step.ready; leftover = 0; prev } :: steps_rev)
       | n -> raise_s [%message "left over bits" (n : int)])
    | desired_width :: rest ->
      (match unused_bits >= desired_width with
       | false ->
         aux
           ~steps_rev:({ Step.ready; leftover = unused_bits; prev } :: steps_rev)
           ~unused_bits:(unused_bits + stream_bits)
           ~remaining
           ~ready:Fields_and_bits.zero
           ~prev:(Fields_and_bits.add prev ready)
       | true ->
         aux
           ~steps_rev
           ~unused_bits:(unused_bits - desired_width)
           ~remaining:rest
           ~ready:(Fields_and_bits.add_field ready ~bits:desired_width)
           ~prev)
  in
  aux
    ~steps_rev:[]
    ~unused_bits:stream_bits
    ~remaining:widths
    ~ready:Fields_and_bits.zero
    ~prev:Fields_and_bits.zero
;;

let%expect_test _ =
  let case widths ~stream_bits =
    let steps = get_steps widths ~stream_bits in
    print_s [%sexp (steps : Step.t list)]
  in
  case [ 8 ] ~stream_bits:8;
  [%expect
    {| (((ready ((fields 1) (data 8))) (leftover 0) (prev ((fields 0) (data 0))))) |}];
  case [ 16 ] ~stream_bits:8;
  [%expect
    {|
    (((ready ((fields 0) (data 0))) (leftover 8) (prev ((fields 0) (data 0))))
     ((ready ((fields 1) (data 16))) (leftover 0) (prev ((fields 0) (data 0)))))
    |}];
  case [ 8; 3; 13; 16 ] ~stream_bits:8;
  [%expect
    {|
    (((ready ((fields 1) (data 8))) (leftover 0) (prev ((fields 0) (data 0))))
     ((ready ((fields 1) (data 3))) (leftover 5) (prev ((fields 1) (data 8))))
     ((ready ((fields 1) (data 13))) (leftover 0) (prev ((fields 2) (data 11))))
     ((ready ((fields 0) (data 0))) (leftover 8) (prev ((fields 3) (data 24))))
     ((ready ((fields 1) (data 16))) (leftover 0) (prev ((fields 3) (data 24)))))
    |}];
  ()
;;

module Make (Config : sig
    val stream_bits : int
    val kind : string

    module Shape : Interface.S
  end) =
struct
  include Config

  module Stream = struct
    type 'a t =
      { data : 'a [@bits stream_bits]
      ; valid : 'a
      ; error : 'a
      ; last : 'a
      }
    [@@deriving hardcaml]
  end

  module Ready =
    Interface.Update
      (Shape)
      (struct
        let port_names_and_widths =
          Shape.map Shape.port_names_and_widths ~f:(fun (name, _) -> name, 1)
        ;;
      end)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; stream : 'a Stream.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a Shape.t
      ; ready : 'a Ready.t
      ; finished : 'a
      ; error : 'a
      ; next : 'a Stream.t
      }
    [@@deriving hardcaml]
  end

  let steps = lazy (get_steps (Shape.port_widths |> Shape.to_list) ~stream_bits)

  module State = struct
    type t =
      | Step of int
      | Complete
    [@@deriving sexp_of, compare ~localize]
  end

  let create scope (i : _ I.t) : _ O.t =
    let steps = Lazy.force steps in
    let num_steps = List.length steps in
    let buffer_size =
      List.map steps ~f:(fun step -> step.leftover)
      |> List.max_elt ~compare:[%compare: int]
      |> Option.value_exn
    in
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create
        (module struct
          include State

          let all = List.init num_steps ~f:(fun i -> State.Step i) @ [ Complete ]
        end)
        reg_spec
    in
    let%hw_var buffer = Always.Variable.reg ~width:buffer_size reg_spec in
    let data_bits = Shape.sum_of_port_widths in
    let ready_bits = Ready.sum_of_port_widths in
    let%hw_var data_raw = Always.Variable.wire ~default:(zero data_bits) () in
    let%hw_var ready_raw = Always.Variable.wire ~default:(zero ready_bits) () in
    let%hw_var error = Always.Variable.wire ~default:gnd () in
    Always.(
      compile
        [ state.switch
            (( Complete
             , [ when_
                   (i.stream.error |: (i.stream.valid &: i.stream.last))
                   [ state.set_next (Step 0) ]
               ] )
             :: List.mapi steps ~f:(fun n step ->
               let is_last = n = num_steps - 1 in
               let total = Fields_and_bits.add step.prev step.ready in
               let next_full = buffer.value @: i.stream.data in
               let for_next_buffer =
                 sel_bottom next_full ~width:step.leftover |> uresize ~width:buffer_size
               in
               let ready_this_turn =
                 drop_bottom next_full ~width:step.leftover
                 |> uresize ~width:step.ready.data
               in
               let emit =
                 proc
                   [ ready_raw
                     <-- zero step.prev.fields
                         @: ones step.ready.fields
                         @: zero (ready_bits - total.fields)
                   ; data_raw
                     <-- zero step.prev.data
                         @: ready_this_turn
                         @: zero (data_bits - total.data)
                   ; buffer <-- for_next_buffer
                   ]
               in
               ( State.Step n
               , [ if_
                     i.stream.error
                     [ error <-- vdd; state.set_next (Step 0) ]
                     [ when_
                         i.stream.valid
                         (if is_last
                          then
                            [ emit
                            ; if_
                                i.stream.last
                                [ state.set_next (Step 0) ]
                                [ state.set_next Complete ]
                            ]
                          else
                            [ if_
                                i.stream.last
                                [ error <-- vdd; state.set_next (Step 0) ]
                                [ emit; state.set_next (Step (n + 1)) ]
                            ])
                     ]
                 ] )))
        ]);
    let%hw.Shape.Of_signal data = Shape.Of_signal.unpack data_raw.value in
    let%hw.Ready.Of_signal ready = Ready.Of_signal.unpack ready_raw.value in
    let%hw finished = state.is Complete in
    let%hw.Stream.Of_signal next =
      Stream.Of_signal.mux finished [ Stream.Of_signal.zero (); i.stream ]
    in
    { data; ready; finished; error = error.value; next }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:[%string "chunk_parser_%{kind}"] create input
  ;;
end
