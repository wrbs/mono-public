(** Several default functions that are shared across various computation pipelines.

    The underlying implementations are also provided here, and exposed from the [Hardcaml]
    module below.

    The non-commutative functions here are carefully written to match their corresponding
    create functions in [node.ml] so that the ordering of arguments lines up. This lets us
    hide this detail from the underlying computation graph and just use lists of unnamed
    arugments. We make this choice so that we don't have to add a different node type for
    every single operator in the graph - you can just add functions to this library. *)

let add = Fixed_op.add
let subtract = Fixed_op.subtract
let multiply = Fixed_op.multiply
let negate = Fixed_op.negate
let div_by_2 = Fixed_op.div_by_2
let concat_lsb = Bitvector.concat_lsb
let select = Bitvector.select
let drop_top = Bitvector.drop_top
let sel_top = Bitvector.sel_top
let drop_bottom = Bitvector.drop_bottom
let sel_bottom = Bitvector.sel_bottom
let bitwise_not = Bitvector.bitwise_not
let bitwise_or = Bitvector.bitwise_or
let bitwise_and = Bitvector.bitwise_and
let mux = Fixed_op.mux
let mux2 = Fixed_op.mux2
let priority_select = Select.priority_select
let priority_select_with_default = Select.priority_select_with_default
let onehot_select = Select.onehot_select
let pipeline = Fixed_op.pipeline
let logical_and = Fixed_op.logical_and
let logical_or = Fixed_op.logical_or
let logical_not = Fixed_op.logical_not
let equals = Fixed_compare.equals
let lt = Fixed_compare.lt
let lte = Fixed_compare.lte
let gt = Fixed_compare.gt
let gte = Fixed_compare.gte
let is_zero = Fixed_compare.is_zero
let is_not_zero = Fixed_compare.is_not_zero
let greater_than_zero = Fixed_compare.greater_than_zero
let abs = Fixed_compare.abs
let max = Fixed_compare.max
let lossless_resize = Resize.lossless
let safe_resize = Resize.safe
let unsafe_resize = Resize.unsafe
let sll = Shift.sll
let sr = Shift.sr
let srl = Shift.srl
let sra = Shift.sra
let divide_by_power = Divide_by_power_node.node
let is_divisible_by_power = Divide_by_power_node.is_divisible
let verify_divide_by_power = Divide_by_power_node.verify
let inverse_lookup_rom = Inverse_lookup_rom_node.node

module Fixed_op = Fixed_op.Op
module Function_tag = Function_tag
module Of_interface = Of_interface
module Wrap_combinational_function = Wrap_combinational_function
module Rom = Rom

module Resize = struct
  open Function_tag.Resize
  module Overflow = Overflow
  module Round = Round
end

(** Expose the underlying hardcaml modules and functions to use directly, rather than
    through the DSL (nodes exposed above). *)
module Hardcaml = struct
  module Fixed_prims = Fixed_prims
  module Divide_by_power = Divide_by_power
end
