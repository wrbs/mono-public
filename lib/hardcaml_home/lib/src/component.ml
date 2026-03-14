open! Core
open! Hardcaml
include Component_intf

module Make (M : Basic) : S with module I := M.I and module O := M.O = struct
  include M

  let create' scope =
    let i_wire = I.Of_signal.wires () in
    create scope i_wire, fun i -> I.Of_signal.assign i_wire i
  ;;

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name create ~scope i
  ;;

  let hierarchical' ?instance scope =
    let i_wire = I.Of_signal.wires () in
    hierarchical ?instance scope i_wire, fun i -> I.Of_signal.assign i_wire i
  ;;
end
