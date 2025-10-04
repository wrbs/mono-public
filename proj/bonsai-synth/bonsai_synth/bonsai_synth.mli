open! Core

include module type of struct
  include Bonsai_synth_core
end

module Midi_value = Midi_value
module Note = Note
module Osc = Osc
module Envelope = Envelope
