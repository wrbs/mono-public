open Core
open Hardcaml

module Make (Arg : Systolic_array_intf.Arg) = struct
  open Arg

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; enable : 'a
      ; input_top : 'a array [@bits element_width] [@length n]
      ; input_left : 'a array [@bits element_width] [@length n]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { processing_element_contents : 'a array [@bits element_width] [@length n * n] }
    [@@deriving hardcaml]
  end

  module Pe_outputs = struct
    type 'a t =
      { right : 'a [@bits element_width]
      ; bottom : 'a [@bits element_width]
      ; contents : 'a [@bits element_width]
      }
    [@@deriving hardcaml]
  end

  let processing_element ~(clocking : _ Clocking.t) ~(enable : Signal.t) ~left ~top =
    let contents, ~right, ~bottom =
      instantiate_processing_element ~clocking ~enable ~left ~top
    in
    { Pe_outputs.contents; right; bottom }
  ;;

  let create (_ : Scope.t) ({ clocking; enable; input_top; input_left } : _ I.t) =
    let maybe_delay ~n x = Clocking.pipeline ~n ~enable clocking x in
    let matrix =
      Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> Pe_outputs.Of_signal.wires ()))
    in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let top =
          match i with
          | 0 -> maybe_delay ~n:j input_top.(j)
          | _ -> matrix.(i - 1).(j).bottom
        in
        let left =
          match j with
          | 0 -> maybe_delay ~n:i input_left.(i)
          | _ -> matrix.(i).(j - 1).right
        in
        Pe_outputs.Of_signal.assign
          matrix.(i).(j)
          (processing_element ~clocking ~enable ~left ~top)
      done
    done;
    let processing_element_contents =
      matrix |> Array.to_list |> Array.concat |> Array.map ~f:(fun x -> x.contents)
    in
    { O.processing_element_contents }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope create i
  ;;
end
