(** Utility functions for handling nullish values (null and undefined) in JavaScript
    bindings *)

open Browser_js_types

(** [opt_case opt ~some] checks if the [Js.opt] is [null] then returns [None] if it is. If
    it's not, it will run [some] on the value and then return it as a [Some] *)
val opt_case : 'a Js.opt -> some:('a -> 'b) -> 'b option

(** [opt_map x ~f] maps ['a option] to ['a Js.opt] and then applies [f] to map ['a Js.opt]
    to ['b Js.opt] *)
val opt_map : 'a option -> f:('a -> 'b) -> 'b Js.opt

(** [optdef_case optdef ~some] checks if the [Js.optdef] is [undefined] then returns
    [None] if it is. If it's not, it will run [some] on the value and then return it as a
    [Some] *)
val optdef_case : 'a Js.optdef -> some:('a -> 'b) -> 'b option

(** [optdef_map x ~f] maps ['a option] to ['a Js.optdef] and then applies [f] to map
    ['a Js.optdef] to ['b Js.optdef] *)
val optdef_map : 'a option -> f:('a -> 'b) -> 'b Js.optdef

(** [opt_optdef_case opt_optdef ~some] checks if the ['a Js.opt Js.optdef] is [undefined]
    or [null] then returns [None] if it is. If it's not, it will run [some] on the ['a]
    and then return it as a [Some 'b] *)
val opt_optdef_case : 'a Js.opt Js.optdef -> some:('a -> 'b) -> 'b option

(** [opt_optdef_to_option opt_optdef] checks if the ['a Js.opt Js.optdef] is [undefined]
    or [null] then returns [None] if it is. If it's not, it returns the ['a] as a
    [Some 'a] *)
val opt_optdef_to_option : 'a Js.opt Js.optdef -> 'a option

(** [opt_optdef_map x ~f] maps ['a option] to ['a Js.optdef] and then applies [f] and
    [Js.Opt.return] to map ['a Js.optdef] to ['b Js.opt Js.optdef] *)
val opt_optdef_map : 'a option -> f:('a -> 'b) -> 'b Js.opt Js.optdef

(** [opt_optdef_of_option x] maps ['a option] to ['a Js.optdef] and then applies
    [Js.Opt.return] to map ['a Js.optdef] to ['a Js.opt Js.optdef] *)
val opt_optdef_of_option : 'a option -> 'a Js.opt Js.optdef
