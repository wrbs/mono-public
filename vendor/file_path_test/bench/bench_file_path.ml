open! Core

(** We do not need to benchmark the [Types] module. *)
module Types = File_path.Types

module Part = Bench_part
module Relative = Bench_relative
module Absolute = Bench_absolute
include Bench_path
module%template [@alloc a = (heap, stack)] Operators = Bench_operators.O [@alloc a]

(** We do not bother benchmarking stable serializations, they use the same code as
    unstable serializations. *)
module Stable = File_path.Stable
