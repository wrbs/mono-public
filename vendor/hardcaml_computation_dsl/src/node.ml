open Core
open Hardcaml

type t =
  | Input of
      { id : int
      ; label : string
      ; precision : Precision.t
      ; latency : int
      }
  | Function of
      { id : int
      ; label : string option
      ; metadata : Function.Metadata.t
      ; shared : bool
      ; inputs : t list
      ; precisions : Precision.t list
      }
  | Constant of
      { id : int
      ; label : string
      ; precision : Precision.t
      ; value : Bits.t option
      }
  | Function_output_buffer of
      { id : int
      ; label : string option
      ; input_function_node : t
      ; input_function_index : int
      ; precision : Precision.t
      }
[@@deriving sexp_of]

let id = ref 0
let reset_ids () = id := 0

let new_id () =
  Int.incr id;
  !id
;;

(* Getters *)

let id = function
  | Input { id; _ }
  | Function { id; _ }
  | Constant { id; _ }
  | Function_output_buffer { id; _ } -> id
;;

let type_ = function
  | Input _ -> "Input"
  | Function { metadata; _ } -> [%string "Function (%{metadata#Function.Metadata})"]
  | Constant { value; _ } ->
    let value_str =
      Option.map value ~f:(fun v ->
        let v = Bits.to_int_trunc v in
        [%string " (%{v#Int})"])
      |> Option.value ~default:""
    in
    [%string "Constant%{value_str}"]
  | Function_output_buffer { input_function_index; _ } ->
    [%string "Function_output_buffer (index %{input_function_index#Int})"]
;;

let label_of_function_node ~label ~(metadata : Function.Metadata.t) =
  match label with
  | Some label -> label
  | None ->
    let legalize_char_for_hardcaml c =
      if Char.equal c ' ' || Char.equal c '\\' || not (Char.is_print c) then '_' else c
    in
    String.map metadata.tag ~f:legalize_char_for_hardcaml
;;

let label = function
  | Input { label; _ } | Constant { label; _ } -> label
  | Function { label; metadata; _ } -> label_of_function_node ~label ~metadata
  | Function_output_buffer { label; input_function_node; _ } ->
    let base_label =
      match input_function_node with
      | Function { label; metadata; _ } -> label_of_function_node ~label ~metadata
      | _ ->
        raise_s
          [%message
            "Function_output_buffer must have a Function as input_function_node"
              (input_function_node : t)]
    in
    (match label with
     | Some label -> label
     | None -> base_label ^ "_output_buffer")
;;

let is_input = function
  | Input _ -> true
  | Function _ | Constant _ | Function_output_buffer _ -> false
;;

let is_constant = function
  | Constant _ -> true
  | Input _ | Function _ | Function_output_buffer _ -> false
;;

let parents = function
  | Input _ | Constant _ -> []
  | Function { inputs; _ } -> inputs
  | Function_output_buffer { input_function_node; _ } -> [ input_function_node ]
;;

let precision = function
  | Input { precision; _ }
  | Constant { precision; _ }
  | Function_output_buffer { precision; _ } -> precision
  | Function _ ->
    raise_s
      [%message
        "Function nodes have multiple precisions; use Function_output_buffer to access \
         individual output precisions"]
;;

let latency = function
  | Input _ -> Some 0
  | Function { metadata = { latency; _ }; _ } -> Some latency
  | Constant _ -> None
  | Function_output_buffer _ -> Some 0
;;

(* Constructors *)

let input ?(latency = 0) ~label ~precision () =
  Input { label; id = new_id (); precision; latency }
;;

let constant_bits ?value ~label ~(precision : Precision.t) () =
  let w = Precision.width precision in
  if w <= 0
  then
    raise_s
      [%message
        "Constant width must be positive!"
          (precision : Precision.t)
          (value : Bits.t option)
          (label : string)];
  Option.iter value ~f:(fun value ->
    if Bits.width value <> w
    then
      raise_s
        [%message
          "Constant width must match the width of the precision!"
            (precision : Precision.t)
            (value : Bits.t)]);
  Constant { label; id = new_id (); precision; value }
;;

let constant ?value ~label ~(precision : Precision.t) () =
  let width = Precision.width precision in
  constant_bits
    ?value:(Option.map ~f:(Bits.of_int_trunc ~width) value)
    ~label
    ~precision
    ()
;;

let unsigned_constant value =
  constant
    ~value
    ~label:("unsigned_constant_" ^ Int.to_string value)
    ~precision:(Hardcaml.Signal.num_bits_to_represent value |> Precision.unsigned_int)
    ()
;;

let generic_function ?label ~(metadata : Function.Metadata.t) ~shared inputs =
  let output_precisions =
    List.map ~f:precision inputs |> metadata.infer_output_precision
  in
  let function_node =
    Function
      { id = new_id (); label; metadata; inputs; shared; precisions = output_precisions }
  in
  List.mapi output_precisions ~f:(fun i precision ->
    let output_buffer_label =
      Option.map label ~f:(fun l -> [%string "%{l}_output_buffer_%{i#Int}"])
    in
    Function_output_buffer
      { id = new_id ()
      ; label = output_buffer_label
      ; input_function_node = function_node
      ; input_function_index = i
      ; precision
      })
;;

let generic_function_single
  ?label
  ~tag
  ~latency
  ~infer_output_precision
  ~create
  ~propagate_input_overflows
  ~shared
  inputs
  =
  generic_function
    ?label
    ~metadata:
      { tag
      ; latency
      ; infer_output_precision = (fun ps -> [ infer_output_precision ps ])
      ; create =
          (fun scope ~clocking ~build_mode ~input_precisions inputs ->
            [ create scope ~clocking ~build_mode ~input_precisions inputs ])
      ; propagate_input_overflows = [ propagate_input_overflows ]
      }
    ~shared
    inputs
  |> List.hd_exn
;;

(* Traversals *)
module Traversal_order = struct
  type t =
    | Pre
    | Post
  [@@deriving equal ~localize]
end

let assert_function_followed_by_output_buffer t =
  let has_function_parent =
    List.exists (parents t) ~f:(fun parent ->
      match parent with
      | Function _ -> true
      | Input _ | Constant _ | Function_output_buffer _ -> false)
  in
  if has_function_parent
  then (
    match t with
    | Function_output_buffer _ -> ()
    | Input _ | Constant _ | Function _ ->
      raise_s
        [%message
          "Every Function node must be followed by a Function_output_buffer node" (t : t)])
;;

let traverse ~order t_list ~f =
  let visited = Int.Hash_set.create () in
  let rec helper ~order t ~f =
    if not (Hash_set.mem visited (id t))
    then (
      Hash_set.add visited (id t);
      assert_function_followed_by_output_buffer t;
      if Traversal_order.equal order Pre then f t;
      (match t with
       | Input _ | Constant _ -> ()
       | Function { inputs; _ } -> List.iter inputs ~f:(helper ~order ~f)
       | Function_output_buffer { input_function_node; _ } ->
         helper ~order input_function_node ~f);
      if Traversal_order.equal order Post then f t)
  in
  List.iter t_list ~f:(helper ~order ~f)
;;

let preorder_traversal = traverse ~order:Pre
let postorder_traversal = traverse ~order:Post
