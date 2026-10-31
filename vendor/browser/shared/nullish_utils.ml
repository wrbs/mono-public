open Core_utils
open Browser_js_types

let return_none () : 'a option = None
let return_some (x : 'a) : 'a option = Some x

(** [opt_case] checks if the [Js.t] is [null] then returns [None] if it is. If it's not,
    it will run [some] on the value and then return it as a [Some] *)
let opt_case (opt : 'a Js.opt) ~(some : 'a -> 'b) : 'b option =
  Js.Opt.case opt return_none (some >> return_some)
;;

(** [opt_map] maps ['a option] to ['a Js.opt] and then applies [f] to map ['a Js.opt] to
    ['b Js.opt] *)
let opt_map (x : 'a option) ~(f : 'a -> 'b) : 'b Js.opt =
  let opt = Js.Opt.option x in
  Js.Opt.map opt f
;;

(** [optdef_case] checks if the [Js.t] is [undefined] then returns [None] if it is. If
    it's not, it will run [some] on the value and then return it as a [Some] *)
let optdef_case (optdef : 'a Js.optdef) ~(some : 'a -> 'b) : 'b option =
  Js.Optdef.case optdef return_none (some >> return_some)
;;

(** [optdef_map] maps ['a option] to ['a Js.optdef] and then applies [f] to map
    ['a Js.optdef] to ['b Js.optdef] *)
let optdef_map (x : 'a option) ~(f : 'a -> 'b) : 'b Js.optdef =
  let optdef = Js.Optdef.option x in
  Js.Optdef.map optdef f
;;

(** [opt_optdef_case] checks if the ['a Js.opt Js.optdef Js.t] is [undefined] or [null]
    then returns [None] if it is. If it's not, it will run [some] on the ['a] and then
    return it as a [Some 'a] *)
let opt_optdef_case (opt_optdef : 'a Js.opt Js.optdef) ~(some : 'a -> 'b) : 'b option =
  Js.Optdef.case opt_optdef return_none (opt_case ~some)
;;

(** [opt_optdef_to_option] checks if the ['a Js.opt Js.optdef Js.t] is [undefined] or
    [null] then returns [None] if it is. If it's not, it returns the ['a] as a [Some 'a] *)
let opt_optdef_to_option (opt_optdef : 'a Js.opt Js.optdef) : 'a option =
  Js.Optdef.case opt_optdef return_none Js.Opt.to_option
;;

(** [opt_optdef_map] maps ['a option] to ['a Js.optdef] and then applies [f] and
    [Js.Opt.return] to map ['a Js.optdef] to ['b Js.opt Js.optdef] *)
let opt_optdef_map (x : 'a option) ~(f : 'a -> 'b) : 'b Js.opt Js.optdef =
  let f = f >> Js.Opt.return in
  optdef_map ~f x
;;

(** [opt_optdef_map] maps ['a option] to ['a Js.optdef] and then applies [Js.Opt.return]
    to map ['a Js.optdef] to ['a Js.opt Js.optdef] *)
let opt_optdef_of_option (x : 'a option) : 'a Js.opt Js.optdef =
  Js.Opt.option x |> Js.Optdef.return
;;
