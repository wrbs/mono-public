open! Core
open! Import
open Incr.Let_syntax

let ( >>> ) f inject b = inject (f b)
let () = Incr.State.(set_max_height_allowed t 1024)

let unzip3_mapi' map ~may_contain_lifecycle ~comparator ~f =
  match (may_contain_lifecycle : May_contain.Lifecycle.t) with
  | No ->
    (* if we know that [f] always returns a triple whose last element (the lifecycle
       incremental) is always the empty lifecycle collection, then we can drop it 
       here, and avoid nesting unzips *)
    let first, second =
      Incr_map.unzip_mapi' map ~f:(fun ~key ~data ->
        let a, b, _ = f ~key ~data in
        a, b)
    in
    first, second, Incr.return (Map.empty comparator)
  | Yes_or_maybe ->
    let first, second_and_third =
      Incr_map.unzip_mapi' map ~f:(fun ~key ~data ->
        let a, b, c = f ~key ~data in
        let bc = Incr.both b c in
        annotate Lifecycle_apply_action_pair bc;
        a, bc)
    in
    let second, third = Incr_map.unzip second_and_third in
    first, second, third
;;

let do_nothing_lifecycle = Incr.return Lifecycle.Collection.empty

let rec gather
  : type result.
    result Computation.t -> (result, unit) Computation.packed_info Trampoline.t
  =
  let open Computation in
  function
  | Return value ->
    let run ~environment ~path:_ ~clock:_ ~model:_ ~inject:_ =
      let result = Value.eval environment value in
      Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path = No
         ; may_contain_lifecycle = No
         })
  | Leaf1 { model; input_id; dynamic_action; apply_action; input; reset } ->
    let wrap_leaf inject = Action.dynamic_leaf >>> inject in
    let run ~environment ~path:_ ~clock:_ ~model ~inject =
      annotate Model model;
      let input = Value.eval environment input in
      (* It's important to create [inject_dynamic] outside of the [let%mapn] so that it
         remains [phys_equal] when the [model] changes. *)
      let inject_dynamic = wrap_leaf inject in
      let result =
        let%mapn model = model in
        model, inject_dynamic
      in
      Trampoline.return
        (Snapshot.create ~result ~input:(Input.dynamic input) ~lifecycle:None, ())
    in
    let apply_action ~inject ~schedule_event input model = function
      | Action.Leaf_static _ ->
        eprint_s
          [%message "BUG: state_machine1's apply_action was called with a dynamic action"];
        model
      | Leaf_dynamic action ->
        apply_action ~inject:(wrap_leaf inject) ~schedule_event input model action
    in
    let reset ~inject ~schedule_event model =
      reset ~inject:(wrap_leaf inject) ~schedule_event model
    in
    Trampoline.return
      (T
         { model
         ; input = input_id
         ; action = Action.Type_id.leaf dynamic_action
         ; apply_action
         ; reset
         ; run
         ; may_contain_path = No
         ; may_contain_lifecycle = No
         })
  | Leaf0 { model; static_action; apply_action; reset } ->
    let wrap_leaf inject = Action.static_leaf >>> inject in
    let run ~environment:_ ~path:_ ~clock:_ ~model ~inject =
      annotate Model model;
      (* It's important to create [inject_static] outside of the [let%mapn] so that it
         remains [phys_equal] when the [model] changes. *)
      let inject_static = wrap_leaf inject in
      let result =
        let%map model = model in
        model, inject_static
      in
      Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
    in
    let apply_action ~inject ~schedule_event _input model = function
      | Action.Leaf_dynamic _ ->
        eprint_s
          [%message "BUG: state_machine0's apply_action was called with a dynamic action"];
        model
      | Leaf_static action ->
        apply_action ~inject:(wrap_leaf inject) ~schedule_event model action
    in
    let reset ~inject ~schedule_event model =
      reset ~inject:(wrap_leaf inject) ~schedule_event model
    in
    Trampoline.return
      (T
         { model
         ; input = Meta.Input.unit
         ; action = Action.Type_id.leaf static_action
         ; apply_action
         ; reset
         ; run
         ; may_contain_path = No
         ; may_contain_lifecycle = No
         })
  | Leaf_incr { input; compute } ->
    let run ~environment ~path:_ ~clock ~model:_ ~inject:_ =
      let input = Value.eval environment input in
      let result = compute clock input in
      Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path = No
         ; may_contain_lifecycle = No
         })
  | Sub { into = Sub { into = Sub _; _ }; _ } as t ->
    Eval_sub.chain t ~gather:{ f = gather }
  | Sub { from; via; into; here } ->
    let%bind.Trampoline (T info_from) = gather from in
    let%bind.Trampoline (T info_into) = gather into in
    Trampoline.return (Eval_sub.gather ~here ~info_from ~info_into ~via)
  | Store { id; value; inner } ->
    let%bind.Trampoline (T gathered) = gather inner in
    let run ~environment ~path ~clock ~model ~inject =
      let value = Value.eval environment value in
      let environment = Environment.add_overwriting environment ~key:id ~data:value in
      gathered.run ~environment ~path ~clock ~model ~inject
    in
    Trampoline.return
      (T
         { run
         ; input = gathered.input
         ; model = gathered.model
         ; action = gathered.action
         ; apply_action = gathered.apply_action
         ; reset = gathered.reset
         ; may_contain_path = gathered.may_contain_path
         ; may_contain_lifecycle = gathered.may_contain_lifecycle
         })
  | Fetch { id; default; for_some } ->
    let run ~environment ~path:_ ~clock:_ ~model:_ ~inject:_ =
      let result =
        match Environment.find environment id with
        | None -> Incr.return default
        | Some x -> Incr.map x ~f:(fun a -> for_some a)
      in
      Trampoline.return (Snapshot.create ~result ~lifecycle:None ~input:Input.static, ())
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path = No
         ; may_contain_lifecycle = No
         })
  | Assoc { map; key_comparator; key_id; cmp_id; data_id; by } ->
    let module Cmp = (val key_comparator) in
    let wrap_assoc ~key inject =
      Action.assoc ~id:key_id ~compare:Cmp.comparator.compare ~key >>> inject
    in
    let%bind.Trampoline (T
                          { model = model_info
                          ; input = input_info
                          ; action
                          ; apply_action
                          ; run
                          ; reset
                          ; may_contain_path
                          ; may_contain_lifecycle
                          })
      =
      gather by
    in
    let run ~environment ~path ~clock ~model ~inject =
      let map_input = Value.eval environment map in
      let input_and_models_map =
        Incr_map.merge map_input model ~f:(fun ~key:_ -> function
          | `Left input -> Some (input, model_info.default)
          | `Right _ -> None
          | `Both input_and_models -> Some input_and_models)
      in
      let create_keyed =
        unstage (Path.Elem.keyed ~compare:Cmp.comparator.compare key_id)
      in
      let results_map, input_map, lifecycle_map =
        unzip3_mapi'
          input_and_models_map
          ~comparator:(module Cmp)
          ~may_contain_lifecycle
          ~f:(fun ~key ~data:input_and_model ->
            annotate Model_and_input input_and_model;
            let path =
              match may_contain_path with
              | Yes_or_maybe -> Path.append path Path.Elem.(Assoc (create_keyed key))
              | No -> path
            in
            let%pattern_bind value, model = input_and_model in
            let key_incr = Incr.const key in
            annotate Assoc_key key_incr;
            annotate Assoc_input value;
            let environment =
              (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
                 since they all start with a fresh "copy" of the outer environment. *)
              environment
              |> Environment.add_exn ~key:key_id ~data:key_incr
              |> Environment.add_exn ~key:data_id ~data:value
            in
            let snapshot, () =
              run ~environment ~path ~clock ~inject:(wrap_assoc ~key inject) ~model
              |> Trampoline.run
            in
            ( Snapshot.result snapshot
            , Input.to_incremental (Snapshot.input snapshot)
            , Snapshot.lifecycle_or_empty snapshot ))
      in
      annotate Assoc_results results_map;
      annotate Assoc_lifecycles lifecycle_map;
      annotate Assoc_inputs input_map;
      let lifecycle =
        (* if we can prove that the body of the assoc doesn't contain a
           lifecycle node, then return None, dropping the constant incremental
           node on the floor. *)
        match may_contain_lifecycle with
        | No -> None
        | Yes_or_maybe ->
          let unfolded =
            Incr_map.unordered_fold_nested_maps
              lifecycle_map
              ~init:Path.Map.empty
              ~add:(fun ~outer_key:_ ~inner_key:key ~data acc ->
                Map.update acc key ~f:(function
                  | Some _ -> Path.raise_duplicate key
                  | None -> data))
              ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Map.remove acc key)
          in
          annotate Assoc_lifecycles unfolded;
          Some unfolded
      in
      Trampoline.return
        ( Snapshot.create ~result:results_map ~input:(Input.dynamic input_map) ~lifecycle
        , () )
    in
    let apply_action
      ~inject
      ~schedule_event
      input
      model
      (Action.Assoc { key; action; id = _; compare = _ })
      =
      let input = Option.bind input ~f:(fun input -> Map.find input key) in
      let specific_model =
        Map.find model key |> Option.value ~default:model_info.default
      in
      let data =
        apply_action
          ~inject:(wrap_assoc ~key inject)
          ~schedule_event
          input
          specific_model
          action
      in
      if model_info.equal data model_info.default
      then Map.remove model key
      else Map.set model ~key ~data
    in
    let reset ~inject ~schedule_event model =
      Map.filter_mapi model ~f:(fun ~key ~data ->
        let new_model = reset ~inject:(wrap_assoc ~key inject) ~schedule_event data in
        if model_info.equal new_model model_info.default then None else Some new_model)
    in
    Trampoline.return
      (T
         { model = Meta.Model.map key_comparator key_id cmp_id model_info
         ; input = Meta.Input.map key_id cmp_id input_info
         ; action = Action.Type_id.assoc ~key:key_id ~action
         ; apply_action
         ; reset
         ; run
         ; may_contain_path
         ; may_contain_lifecycle
         })
  | Assoc_on
      { map
      ; io_comparator
      ; model_comparator
      ; io_key_id
      ; io_cmp_id
      ; model_key_id
      ; model_cmp_id
      ; data_id
      ; by
      ; get_model_key
      } ->
    let module Model_comparator = (val model_comparator) in
    let module Io_comparator = (val io_comparator) in
    let wrap_assoc_on ~io_key ~model_key inject =
      Action.assoc_on
        ~io_key
        ~model_key
        ~io_id:io_key_id
        ~io_compare:Io_comparator.comparator.compare
      >>> inject
    in
    let model_key_comparator = Model_comparator.comparator in
    let%bind.Trampoline (T
                          { model = model_info
                          ; input = input_info
                          ; action
                          ; apply_action
                          ; run
                          ; reset
                          ; may_contain_path
                          ; may_contain_lifecycle
                          })
      =
      gather by
    in
    let run ~environment ~path ~clock ~model ~inject =
      let map_input = Value.eval environment map in
      let model_lookup = Incr_map.Lookup.create model ~comparator:model_key_comparator in
      let create_keyed =
        unstage (Path.Elem.keyed ~compare:Io_comparator.comparator.compare io_key_id)
      in
      let results_map, input_map, lifecycle_map =
        unzip3_mapi'
          map_input
          ~may_contain_lifecycle
          ~comparator:(module Io_comparator)
          ~f:(fun ~key:io_key ~data:value ->
            let%pattern_bind results_map, input_map, lifecycle_map =
              let path =
                match may_contain_path with
                | Yes_or_maybe -> Path.append path Path.Elem.(Assoc (create_keyed io_key))
                | No -> path
              in
              let key_incr = Incr.const io_key in
              annotate Assoc_key key_incr;
              annotate Assoc_input value;
              let environment =
                (* It is safe to reuse the same [key_id] and [data_id] for each pair in the map,
                   since they all start with a fresh "copy" of the outer environment. *)
                environment
                |> Environment.add_exn ~key:io_key_id ~data:key_incr
                |> Environment.add_exn ~key:data_id ~data:value
              in
              let model_key =
                let%map value = value in
                get_model_key io_key value
              in
              Incr.set_cutoff
                model_key
                (Incr.Cutoff.of_compare model_key_comparator.compare);
              let%bind model_key = model_key in
              let model =
                match%map Incr_map.Lookup.find model_lookup model_key with
                | None -> model_info.default
                | Some (_prev_io_key, model) -> model
              in
              annotate Model model;
              let snapshot, () =
                run
                  ~environment
                  ~path
                  ~clock
                  ~inject:(wrap_assoc_on ~io_key ~model_key inject)
                  ~model
                |> Trampoline.run
              in
              let%mapn result = Snapshot.result snapshot
              and input = Input.to_incremental (Snapshot.input snapshot)
              and lifecycle = Snapshot.lifecycle_or_empty snapshot in
              result, input, lifecycle
            in
            results_map, input_map, lifecycle_map)
      in
      annotate Assoc_results results_map;
      annotate Assoc_lifecycles lifecycle_map;
      let lifecycle =
        (* if we can prove that the body of the assoc_on doesn't contain a
           lifecycle node, then return None, dropping the constant incremental
           node on the floor. *)
        match may_contain_lifecycle with
        | No -> None
        | Yes_or_maybe ->
          let unfolded =
            Incr_map.unordered_fold_nested_maps
              lifecycle_map
              ~init:Path.Map.empty
              ~add:(fun ~outer_key:_ ~inner_key:key ~data acc ->
                Map.update acc key ~f:(function
                  | Some _ -> Path.raise_duplicate key
                  | None -> data))
              ~remove:(fun ~outer_key:_ ~inner_key:key ~data:_ acc -> Map.remove acc key)
          in
          annotate Assoc_lifecycles unfolded;
          Some unfolded
      in
      Trampoline.return
        ( Snapshot.create ~result:results_map ~input:(Input.dynamic input_map) ~lifecycle
        , () )
    in
    let apply_action
      ~inject
      ~schedule_event
      input
      model
      (Action.Assoc_on { io_key; model_key; action; io_id = _; io_compare = _ })
      =
      let input = Option.bind input ~f:(fun input -> Map.find input io_key) in
      let specific_model =
        match Map.find model model_key with
        | None -> model_info.default
        | Some (_prev_io_key, model) -> model
      in
      let new_model =
        apply_action
          ~inject:(wrap_assoc_on ~io_key ~model_key inject)
          ~schedule_event
          input
          specific_model
          action
      in
      if model_info.equal new_model model_info.default
      then Map.remove model model_key
      else Map.set model ~key:model_key ~data:(io_key, new_model)
    in
    let reset ~inject ~schedule_event model =
      Map.filter_mapi model ~f:(fun ~key:model_key ~data:(io_key, model) ->
        let new_model =
          reset ~inject:(wrap_assoc_on ~io_key ~model_key inject) ~schedule_event model
        in
        if model_info.equal new_model model_info.default
        then None
        else Some (io_key, new_model))
    in
    Trampoline.return
      (T
         { model =
             Meta.Model.map_on
               model_comparator
               io_comparator
               model_key_id
               io_key_id
               model_cmp_id
               model_info
         ; input = Meta.Input.map io_key_id io_cmp_id input_info
         ; action =
             Action.Type_id.assoc_on ~io_key:io_key_id ~model_key:model_key_id ~action
         ; apply_action
         ; reset
         ; run
         ; may_contain_path
         ; may_contain_lifecycle
         })
  | Assoc_simpl { map; by; may_contain_path } ->
    let run ~environment ~path ~clock:_ ~model:_ ~inject:_ =
      let map_input = Value.eval environment map in
      let result = Incr_map.mapi map_input ~f:(fun ~key ~data -> by path key data) in
      Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path
         ; may_contain_lifecycle = No
         })
  | Switch { match_; arms; here = _ } ->
    let wrap_switch ~branch ~type_id inject = Action.switch ~branch ~type_id >>> inject in
    let%bind.Trampoline gathered = Trampoline.all_map (Map.map arms ~f:gather) in
    let may_contain_lifecycle =
      Map.fold
        gathered
        ~init:May_contain.Lifecycle.No
        ~f:(fun ~key:_ ~data:(T gathered) acc ->
        May_contain.Lifecycle.merge acc gathered.may_contain_lifecycle)
    in
    let may_contain_path, needs_disambiguation =
      let num_contain_path =
        Map.count gathered ~f:(function
          | T { may_contain_path = Yes_or_maybe; _ } -> true
          | T { may_contain_path = No; _ } -> false)
      in
      let may_contain_path =
        if num_contain_path > 0 then May_contain.Path.Yes_or_maybe else No
      in
      may_contain_path, num_contain_path > 1
    in
    let run ~environment ~path ~clock ~model ~inject =
      annotate Model model;
      let index = Value.eval environment match_ in
      let result_input_and_lifecycle =
        let%bind index = index in
        (* !!!This is a load-bearing bind!!!

           If this bind isn't here, the scope that is created for the bind
           doesn't exist, and old incremental nodes might still be active, and
           with things like [match%sub] or [Bonsai.match_either] can witness old
           nodes, which can cause [assert false] to trigger. *)
        let path =
          if needs_disambiguation then Path.append path (Path.Elem.Switch index) else path
        in
        let (T
              { model = model_info
              ; input = input_info
              ; action = action_info
              ; apply_action = _
              ; reset = _
              ; may_contain_path = _
              ; may_contain_lifecycle = _
              ; run
              })
          =
          Map.find_exn gathered index
        in
        let chosen_model =
          Incremental.map model ~f:(fun map ->
            let (Meta.Model.Hidden.T { model; info }) =
              Meta.Multi_model.find_exn map index
            in
            let equal =
              Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
            in
            Type_equal.conv equal model)
        in
        let snapshot, () =
          run
            ~environment
            ~model:chosen_model
            ~path
            ~clock
            ~inject:(wrap_switch ~type_id:action_info ~branch:index inject)
          |> Trampoline.run
        in
        let input =
          let%mapn input = Input.to_incremental (Snapshot.input snapshot) in
          Meta.Input.Hidden.T { input; type_id = input_info; key = index }
        in
        Incr.return (Snapshot.result snapshot, input, Snapshot.lifecycle_or_empty snapshot)
      in
      let result = Incr.bind result_input_and_lifecycle ~f:Tuple3.get1
      and input = Incr.bind result_input_and_lifecycle ~f:Tuple3.get2
      and lifecycle = Incr.bind result_input_and_lifecycle ~f:Tuple3.get3 in
      let input = Input.dynamic input in
      let lifecycle =
        (* if we can prove that none of the switch cases have lifecycle functions, 
           then return None, dropping the incremental node on the floor. *)
        match may_contain_lifecycle with
        | No -> None
        | Yes_or_maybe -> Some lifecycle
      in
      Trampoline.return (Snapshot.create ~result ~input ~lifecycle, ())
    in
    let apply_action
      ~inject
      ~schedule_event
      input
      model
      (Action.Switch { action; type_id = action_type_id; branch = index })
      =
      let (T
            { model = tm
            ; input = im
            ; action = am
            ; apply_action
            ; run = _
            ; reset = _
            ; may_contain_path = _
            ; may_contain_lifecycle = _
            })
        =
        Map.find_exn gathered index
      in
      let (T { model = chosen_model; info = chosen_model_info; _ }) =
        Meta.Multi_model.find_exn model index
      in
      match
        ( Action.Type_id.same_witness action_type_id am
        , Meta.Model.Type_id.same_witness chosen_model_info.type_id tm.type_id )
      with
      | Some T, Some T ->
        let new_model =
          match input with
          | Some
              (Meta.Input.Hidden.T
                { input = chosen_input; type_id = chosen_input_info; key = index' }) ->
            (match index = index', Meta.Input.same_witness chosen_input_info im with
             | true, Some T ->
               apply_action
                 ~inject:(wrap_switch ~type_id:am ~branch:index inject)
                 ~schedule_event
                 (Some chosen_input)
                 chosen_model
                 action
             | _ ->
               apply_action
                 ~inject:(wrap_switch ~type_id:am ~branch:index inject)
                 ~schedule_event
                 None
                 chosen_model
                 action)
          | None ->
            apply_action
              ~inject:(wrap_switch ~type_id:am ~branch:index inject)
              ~schedule_event
              None
              chosen_model
              action
        in
        let new_model = Meta.Model.Hidden.create tm new_model in
        Meta.Multi_model.set model ~key:index ~data:new_model
      | None, None | Some T, None | None, Some T ->
        let action = Action.Type_id.to_sexp action_type_id action in
        eprint_s
          [%message
            "an action inside of Bonsai.switch has been dropped because the computation \
             is no longer active"
              (index : int)
              (action : Sexp.t)];
        model
    in
    let reset ~inject ~schedule_event model =
      let f ~key:index ~data:(model : Meta.Model.Hidden.t) =
        let (T { model = chosen_model; info = chosen_model_info; _ }) = model in
        let (T
              { model = tm
              ; input = _
              ; action = am
              ; reset
              ; apply_action = _
              ; may_contain_lifecycle = _
              ; run = _
              ; may_contain_path = _
              })
          =
          Map.find_exn gathered index
        in
        let T =
          Meta.Model.Type_id.same_witness_exn tm.type_id chosen_model_info.type_id
        in
        let new_model =
          reset
            ~inject:(wrap_switch ~type_id:am ~branch:index inject)
            ~schedule_event
            chosen_model
        in
        Meta.Model.Hidden.create tm new_model
      in
      Meta.Multi_model.of_models (Map.mapi (Meta.Multi_model.to_models model) ~f)
    in
    let model =
      let models =
        Map.map gathered ~f:(fun (T { model; _ }) ->
          Meta.Model.Hidden.create model model.default)
      in
      Meta.Multi_model.model_info (Meta.Multi_model.of_models models)
    in
    Trampoline.return
      (T
         { model
         ; input = Meta.Input.Hidden.int
         ; action = Action.Type_id.switch
         ; apply_action
         ; reset
         ; run
         ; may_contain_path
         ; may_contain_lifecycle
         })
  | Lazy lazy_computation ->
    let wrap_lazy ~type_id inject = Action.lazy_ ~type_id >>> inject in
    let model = Meta.Model.Hidden.lazy_ in
    let gathered = lazy_computation |> Lazy.map ~f:(fun c -> Trampoline.run (gather c)) in
    let run ~environment ~path ~clock ~model ~inject =
      let (T
            { model = model_info
            ; input = input_info
            ; action = action_info
            ; run
            ; apply_action = _
            ; reset = _
            ; may_contain_path = _
            ; may_contain_lifecycle = _
            })
        =
        force gathered
      in
      annotate Model model;
      let input_model =
        let%map model = model in
        let (Meta.Model.Hidden.T { model; info; _ }) =
          Option.value
            model
            ~default:(Meta.Model.Hidden.create model_info model_info.default)
        in
        let witness =
          Meta.Model.Type_id.same_witness_exn info.type_id model_info.type_id
        in
        Type_equal.conv witness model
      in
      let%bind.Trampoline snapshot, () =
        run
          ~environment
          ~path
          ~clock
          ~model:input_model
          ~inject:(wrap_lazy ~type_id:action_info inject)
      in
      let input =
        Input.map (Snapshot.input snapshot) ~f:(fun input ->
          Meta.Input.Hidden.T { input; type_id = input_info; key = () })
      in
      Trampoline.return
        ( Snapshot.create
            ~input
            ~result:(Snapshot.result snapshot)
            ~lifecycle:(Snapshot.lifecycle snapshot)
        , () )
    in
    let apply_action
      ~inject
      ~schedule_event
      input
      model
      (Action.Lazy { action; type_id = action_type_id })
      =
      (* forcing the lazy is fine because actions are finite in length *)
      let (T
            { model = model_info
            ; input = input_info
            ; action = action_info
            ; apply_action
            ; run = _
            ; reset = _
            ; may_contain_path = _
            ; may_contain_lifecycle = _
            })
        =
        force gathered
      in
      let (Meta.Model.Hidden.T { model = chosen_model; info = chosen_model_info; _ }) =
        Option.value
          model
          ~default:(Meta.Model.Hidden.create model_info model_info.default)
      in
      let T = Action.Type_id.same_witness_exn action_type_id action_info in
      let T =
        Meta.Model.Type_id.same_witness_exn chosen_model_info.type_id model_info.type_id
      in
      let new_model =
        match input with
        | Some (Meta.Input.Hidden.T { input; type_id = input_type_id; key = () }) ->
          let T = Meta.Input.same_witness_exn input_type_id input_info in
          apply_action
            ~inject:(wrap_lazy ~type_id:action_info inject)
            ~schedule_event
            (Some input)
            chosen_model
            action
        | None ->
          apply_action
            ~inject:(wrap_lazy ~type_id:action_info inject)
            ~schedule_event
            None
            chosen_model
            action
      in
      Some (Meta.Model.Hidden.create model_info new_model)
    in
    let reset' ~inject ~schedule_event model =
      let (T
            { model = model_info
            ; action = action_info
            ; reset
            ; apply_action = _
            ; run = _
            ; input = _
            ; may_contain_path = _
            ; may_contain_lifecycle = _
            })
        =
        force gathered
      in
      let (Meta.Model.Hidden.T { model = chosen_model; info = chosen_model_info; _ }) =
        model
      in
      let T =
        Meta.Model.Type_id.same_witness_exn chosen_model_info.type_id model_info.type_id
      in
      let new_model =
        reset ~inject:(wrap_lazy ~type_id:action_info inject) ~schedule_event chosen_model
      in
      Meta.Model.Hidden.create model_info new_model
    in
    let reset ~inject ~schedule_event model =
      (* If the model is None, then you can't descend into the reset because it will
         force the lazy, but that doesn't matter because there's nothing to reset anyway. *)
      match model with
      | None -> None
      | Some model -> Some (reset' ~inject ~schedule_event model)
    in
    Trampoline.return
      (T
         { model
         ; input = Meta.Input.Hidden.unit
         ; action = Action.Type_id.lazy_
         ; apply_action
         ; run
         ; reset
         ; may_contain_path = Yes_or_maybe
         ; may_contain_lifecycle = Yes_or_maybe
         })
  | Wrap
      { wrapper_model
      ; action_id
      ; result_id
      ; inject_id
      ; model_id
      ; inner
      ; dynamic_apply_action
      ; reset = reset_me
      } ->
    let%bind.Trampoline (T
                          { model = inner_model
                          ; input = inner_input
                          ; action = inner_action
                          ; apply_action
                          ; run
                          ; reset
                          ; may_contain_path
                          ; may_contain_lifecycle
                          })
      =
      gather inner
    in
    let wrap_inner inject = Action.wrap_inner >>> inject in
    let wrap_outer inject = Action.wrap_outer >>> inject in
    let run ~environment ~path ~clock ~model ~inject =
      annotate Model model;
      let%pattern_bind outer_model, inner_model = model in
      annotate Model outer_model;
      let%bind.Trampoline inner_snapshot, () =
        let environment =
          environment
          |> Environment.add_exn ~key:model_id ~data:outer_model
          |> Environment.add_exn ~key:inject_id ~data:(Incr.return (wrap_outer inject))
        in
        run ~environment ~path ~model:inner_model ~clock ~inject:(wrap_inner inject)
      in
      let inner_result = Snapshot.result inner_snapshot in
      let input =
        Input.merge
          (Snapshot.input inner_snapshot)
          (Input.dynamic (Snapshot.result inner_snapshot))
      in
      Trampoline.return
        ( Snapshot.create
            ~result:inner_result
            ~input
            ~lifecycle:(Snapshot.lifecycle inner_snapshot)
        , () )
    in
    let model = Meta.Model.both wrapper_model inner_model in
    let apply_action ~inject ~schedule_event input (outer_model, inner_model) action =
      match action with
      | Action.Wrap_outer action_outer ->
        let new_outer_model =
          dynamic_apply_action
            ~inject:(wrap_outer inject)
            ~schedule_event
            (Option.map input ~f:snd)
            outer_model
            action_outer
        in
        new_outer_model, inner_model
      | Wrap_inner action_inner ->
        let new_inner_model =
          apply_action
            ~inject:(wrap_inner inject)
            ~schedule_event
            (Option.map input ~f:fst)
            inner_model
            action_inner
        in
        outer_model, new_inner_model
    in
    let reset ~inject ~schedule_event (outer_model, inner_model) =
      let outer_model =
        reset_me ~inject:(wrap_outer inject) ~schedule_event outer_model
      in
      let inner_model = reset ~inject:(wrap_inner inject) ~schedule_event inner_model in
      outer_model, inner_model
    in
    Trampoline.return
      (T
         { model
         ; input = Meta.Input.both inner_input result_id
         ; action = Action.Type_id.wrap ~inner:inner_action ~outer:action_id
         ; apply_action
         ; run
         ; reset
         ; may_contain_path
         ; may_contain_lifecycle
         })
  | With_model_resetter { inner; reset_id } ->
    let%bind.Trampoline (T
                          ({ model
                           ; input
                           ; action
                           ; apply_action
                           ; run
                           ; reset
                           ; may_contain_path
                           ; may_contain_lifecycle
                           } as gathered_inner))
      =
      gather inner
    in
    let inner_stateless =
      let same_model = Meta.Model.Type_id.same_witness in
      let same_action = Action.Type_id.same_witness in
      let open Option.Let_syntax in
      let%bind a = same_model model.type_id Meta.Model.unit.type_id in
      let%bind b = same_action action Action.Type_id.nothing in
      Some (a, b)
    in
    (match inner_stateless with
     | Some (T, T) ->
       let run ~environment:env ~path ~clock ~model:_ ~inject:_ =
         let environment = Environment.add_exn ~key:reset_id ~data:ignore_effect env in
         run ~environment ~path ~clock ~model:unit_model ~inject:unreachable_action
       in
       Trampoline.return (T { gathered_inner with run })
     | None ->
       let wrap_inner inject = Action.model_reset_inner >>> inject in
       let run ~environment ~path ~clock ~model ~inject =
         let environment =
           environment
           |> Environment.add_exn
                ~key:reset_id
                ~data:(Incr.return (inject Action.model_reset_outer))
         in
         let%bind.Trampoline snapshot, () =
           run ~environment ~path ~model ~clock ~inject:(wrap_inner inject)
         in
         let result = Snapshot.result snapshot in
         Trampoline.return
           ( Snapshot.create
               ~result
               ~input:(Snapshot.input snapshot)
               ~lifecycle:(Snapshot.lifecycle snapshot)
           , () )
       in
       let apply_action ~inject ~schedule_event i m = function
         | Action.Model_reset_outer -> reset ~inject:(wrap_inner inject) ~schedule_event m
         | Model_reset_inner inner ->
           apply_action ~inject:(wrap_inner inject) ~schedule_event i m inner
       in
       let reset ~inject ~schedule_event m =
         reset ~inject:(wrap_inner inject) ~schedule_event m
       in
       Trampoline.return
         (T
            { model
            ; input
            ; action = Action.Type_id.model_reset action
            ; apply_action
            ; run
            ; reset
            ; may_contain_path
            ; may_contain_lifecycle
            }))
  | Path ->
    let run ~environment:_ ~path ~clock:_ ~model:_ ~inject:_ =
      let result = Incr.return path in
      annotate Path result;
      Trampoline.return (Snapshot.create ~result ~input:Input.static ~lifecycle:None, ())
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path = Yes_or_maybe
         ; may_contain_lifecycle = No
         })
  | Lifecycle lifecycle ->
    let run ~environment ~path ~clock:_ ~model:_ ~inject:_ =
      let lifecycle =
        match%pattern_bind Value.eval environment lifecycle with
        | Some lifecycle ->
          let%map lifecycle = lifecycle in
          Path.Map.singleton path lifecycle
        | None -> do_nothing_lifecycle
      in
      Trampoline.return
        ( Snapshot.create
            ~result:(Incr.return ())
            ~input:Input.static
            ~lifecycle:(Some lifecycle)
        , () )
    in
    Trampoline.return
      (T
         { model = Meta.Model.unit
         ; input = Meta.Input.unit
         ; action = Action.Type_id.nothing
         ; apply_action = unusable_apply_action
         ; reset = reset_unit_model
         ; run
         ; may_contain_path = Yes_or_maybe
         ; may_contain_lifecycle = Yes_or_maybe
         })
;;

let gather c = Trampoline.run (gather c)
