open! Core
module View = View
module Attr = Attr
module Event = Event
module Effect = Effect
module Cursor = Cursor
module Captured_or_ignored = Captured_or_ignored
include Geom

let start_with_exit = Loop.start_with_exit
let start = Loop.start

module Bonsai = Bonsai

module Private = struct
  module Driver = Driver
  module Frame_outcome = Frame_outcome

  module For_testing = struct
    let make_app_exit_on_ctrlc = Loop.For_testing.make_app_exit_on_ctrlc
    let with_driver = Loop.For_testing.with_driver
  end
end
