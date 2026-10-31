open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; enable : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { count : 'a [@bits 32] } [@@deriving hardcaml]
end

let create _scope i =
  let { I.clock; enable } = i in
  let spec = Reg_spec.create ~clock () in
  let count = reg_fb spec ~width:32 ~enable ~f:(fun count -> count +:. 1) in
  { O.count }
;;
