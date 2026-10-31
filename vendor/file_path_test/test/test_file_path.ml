open! Core

(** We do not need to test the [Types] module. *)
module Types = File_path.Types

module Absolute = Test_absolute
module Relative = Test_relative
module Part = Test_part
include Test_path
module%template [@alloc a = (heap, stack)] Operators = Test_operators.O [@alloc a]
module Stable = Test_stable
