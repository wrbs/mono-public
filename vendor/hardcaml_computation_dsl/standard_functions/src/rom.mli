open Hardcaml
open Hardcaml_computation_dsl

module Make (X : Interface_with_precision.S) : sig
  val create : Bits.t X.t array -> latency:int -> read_address:Node.t -> Node.t X.t
end
