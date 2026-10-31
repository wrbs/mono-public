open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock_a : 'a
    ; clock_b : 'a
    ; enable : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { count_a : 'a [@bits 32]
    ; count_b : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

let create _scope (i : _ I.t) =
  let spec_a = Reg_spec.create ~clock:i.clock_a () in
  let spec_b = Reg_spec.create ~clock:i.clock_b () in
  let count_a = reg_fb spec_a ~width:32 ~enable:i.enable ~f:(fun c -> c +:. 1) in
  let count_b = reg_fb spec_b ~width:32 ~enable:i.enable ~f:(fun c -> c +:. 1) in
  { O.count_a; count_b }
;;
