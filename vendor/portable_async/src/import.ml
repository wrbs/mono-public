open! Core
include Async_kernel

let encapsulated_execution_context () =
  Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
;;
