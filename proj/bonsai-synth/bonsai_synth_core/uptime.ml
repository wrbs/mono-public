open! Core
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect

type t = Time_ns.t

let since_start t = Time_ns.to_span_since_epoch t
let sexp_of_t t = [%sexp (since_start t : Time_ns.Span.t)]
let of_span_since_start span = Time_ns.of_span_since_epoch span
let secs_since_start t = Time_ns.Span.to_sec (since_start t)
let sub t t' = Time_ns.diff t t'
let add = Time_ns.add
let current = Bonsai.Clock.Expert.now
let approximate = Bonsai.Clock.approx_now
let at = Bonsai.Clock.at
let get_current = Bonsai.Clock.get_current_time
let sleep = Bonsai.Clock.sleep
let until = Bonsai.Clock.until

module Expert = struct
  let to_time_ns t = t
  let of_time_ns t = t
end
