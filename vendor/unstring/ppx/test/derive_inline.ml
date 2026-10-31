open! Core
open! Unstring.Export

[@@@expand_inline
  let _ : (y:int * x:string) Unstring.t = [%unstring ("%{y} %{x}" : y:int * x:string)]]

let _ : (y:int * x:string) Unstring.t =
  Unstring.map_hlist
    [ ((unstring_int
       : _ Unstring.t)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
    ; Unstring.lit " "
    ; ((unstring_string
       : _ Unstring.t)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
    ]
    ~f:(fun [ __arg_y; (); __arg_x ] -> ~y:__arg_y, ~x:__arg_x) [@merlin.hide]
;;

[@@@end]

type t = { age : int } [@@deriving_inline unstring ~restring] [@@format "%{age}"]

let _ = fun (_ : t) -> ()

let unstring =
  Unstring.map_hlist
    [ ((unstring_int
       : _ Unstring.t)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
    ]
    ~f:(fun (__arg_age :: []) -> { age = __arg_age })
;;

let _ = unstring

let restring = function
  | { age = __field_0 } ->
    ((restring_int
     : _ -> string)
     [@error_message
       " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
        \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
      __field_0
;;

let _ = restring

[@@@end]

type tagged =
  [ `Int of (int[@format "int:%{_}"])
  | `Str of (string[@format "str:%{_}"])
  ]
[@@deriving_inline unstring ~restring]

let _ = fun (_ : tagged) -> ()

let unstring_tagged =
  ((Unstring.choice
      [ Unstring.map
          (((fun __inner ->
              Unstring.map_hlist
                [ Unstring.lit "int:"; __inner ]
                ~f:(fun [ (); __arg_1 ] -> __arg_1))
              unstring_int
           : _ Unstring.t)
           [@error_message
             " Hint: some types require attributes; did you forget [@some \"...\"], \
              [@none \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
          ~f:(fun x -> `Int x)
      ; Unstring.map
          (((fun __inner ->
              Unstring.map_hlist
                [ Unstring.lit "str:"; __inner ]
                ~f:(fun [ (); __arg_1 ] -> __arg_1))
              unstring_string
           : _ Unstring.t)
           [@error_message
             " Hint: some types require attributes; did you forget [@some \"...\"], \
              [@none \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
          ~f:(fun x -> `Str x)
      ]
   : _ Unstring.t)
   [@error_message
     " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
      \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
;;

let _ = unstring_tagged

let restring_tagged =
  ((function
    | `Int __x ->
      (((fun __f ->
          ();
          fun __x -> String.concat ~sep:"" [ "int:"; __f __x ])
          restring_int
       : _ -> string)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
        __x
    | `Str __x ->
      (((fun __f ->
          ();
          fun __x -> String.concat ~sep:"" [ "str:"; __f __x ])
          restring_string
       : _ -> string)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
        __x
   : _ -> string)
   [@error_message
     " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
      \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
;;

let _ = restring_tagged

[@@@end]

[@@@expand_inline
  let _ : (y:int * x:string) Unstring.t = [%unstring ("%{y} %{x}" : y:int * x:string)]]

let _ : (y:int * x:string) Unstring.t =
  Unstring.map_hlist
    [ ((unstring_int
       : _ Unstring.t)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
    ; Unstring.lit " "
    ; ((unstring_string
       : _ Unstring.t)
       [@error_message
         " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
          \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
    ]
    ~f:(fun [ __arg_y; (); __arg_x ] -> ~y:__arg_y, ~x:__arg_x) [@merlin.hide]
;;

[@@@end]

[@@@expand_inline
  let very_complex_example =
    [%unstring
      ("host=%{host} [%{2}] status=%{kind} (metric: %{1})"
       : int
         * kind:[ `Error of (string * code:int[@format "error(%{_}, code=%{code})"])
                | `Warning of (string[@format "warn: %{_}"])
                | `Info [@format "info"]
                ]
         * host:string
         * float)]
  ;;]

let very_complex_example =
  (Unstring.map_hlist
     [ Unstring.lit "host="
     ; ((unstring_string
        : _ Unstring.t)
        [@error_message
          " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
           \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
     ; Unstring.lit " ["
     ; ((unstring_float
        : _ Unstring.t)
        [@error_message
          " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
           \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
     ; Unstring.lit "] status="
     ; ((Unstring.choice
           [ Unstring.map
               ((Unstring.map_hlist
                   [ Unstring.lit "error("
                   ; ((unstring_string
                      : _ Unstring.t)
                      [@error_message
                        " Hint: some types require attributes; did you forget [@some \
                         \"...\"], [@none \"...\"], [@unit \"...\"], [@format \"...\"], \
                         or [@sep \"...\"]?"])
                   ; Unstring.lit ", code="
                   ; ((unstring_int
                      : _ Unstring.t)
                      [@error_message
                        " Hint: some types require attributes; did you forget [@some \
                         \"...\"], [@none \"...\"], [@unit \"...\"], [@format \"...\"], \
                         or [@sep \"...\"]?"])
                   ; Unstring.lit ")"
                   ]
                   ~f:(fun [ (); __arg_1; (); __arg_code; () ] ->
                     __arg_1, ~code:__arg_code)
                : _ Unstring.t)
                [@error_message
                  " Hint: some types require attributes; did you forget [@some \"...\"], \
                   [@none \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \
                   \"...\"]?"])
               ~f:(fun x -> `Error x)
           ; Unstring.map
               (((fun __inner ->
                   Unstring.map_hlist
                     [ Unstring.lit "warn: "; __inner ]
                     ~f:(fun [ (); __arg_1 ] -> __arg_1))
                   unstring_string
                : _ Unstring.t)
                [@error_message
                  " Hint: some types require attributes; did you forget [@some \"...\"], \
                   [@none \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \
                   \"...\"]?"])
               ~f:(fun x -> `Warning x)
           ; Unstring.map (Unstring.all_unit [ Unstring.lit "info" ]) ~f:(fun () -> `Info)
           ]
        : _ Unstring.t)
        [@error_message
          " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
           \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
     ; Unstring.lit " (metric: "
     ; ((unstring_int
        : _ Unstring.t)
        [@error_message
          " Hint: some types require attributes; did you forget [@some \"...\"], [@none \
           \"...\"], [@unit \"...\"], [@format \"...\"], or [@sep \"...\"]?"])
     ; Unstring.lit ")"
     ]
     ~f:(fun [ (); __arg_host; (); __arg_2; (); __arg_kind; (); __arg_1; () ] ->
       __arg_1, ~kind:__arg_kind, ~host:__arg_host, __arg_2) [@merlin.hide])
;;

[@@@end]

let%expect_test "very complex example run" =
  let of_string =
    Unstring.make_of_string
      very_complex_example
      ~compile_when:`Now
      ~on_ambiguity:(`Raise sexp_of_opaque)
    |> Staged.unstage
  in
  let result = of_string "host=foo [12.6] status=error(oh no, code=15) (metric: 2)" in
  print_s
    [%sexp
      (result
       : int
         * kind:[ `Error of string * code:int | `Info | `Warning of string ]
         * host:string
         * float)];
  [%expect {| ((. 2) (~kind (Error ((. "oh no") (~code 15)))) (~host foo) (. 12.6)) |}]
;;
