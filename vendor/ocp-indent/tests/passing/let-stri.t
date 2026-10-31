Special let-in constructs should be intended similarly to regular let-in
and consistently with their structure item form.
See a standard let-in for reference:

  $ cat > test.ml << EOF
  > let
  > x
  > =
  > 2
  > 
  > let x =
  > let
  > y
  > =
  > 2
  > in
  > y + 2
  > 
  > let x =
  > let y =
  > 2
  > in
  > y + 2
  > EOF

  $ ocp-indent test.ml
  let
    x
    =
    2
  
  let x =
    let
      y
      =
      2
    in
    y + 2
  
  let x =
    let y =
      2
    in
    y + 2

1. let module _ in (inconsistent with the stri)

  $ cat > test.ml << EOF
  > module
  > M
  > =
  > struct
  > let x = 2
  > end
  > 
  > module
  > N
  > =
  > List
  > 
  > let x =
  > let
  > module
  > M
  > =
  > struct
  > let x = 2
  > end
  > in
  > let
  > module
  > N
  > =
  > List
  > in
  > N.singleton M.x
  > 
  > let x =
  > let module M = struct
  > let x = 2
  > end
  > in
  > let module N =
  > List
  > in
  > N.singleton M.x
  > EOF

  $ ocp-indent test.ml
  module
    M
  =
  struct
    let x = 2
  end
  
  module
    N
    =
    List
  
  let x =
    let
      module
      M
    =
    struct
      let x = 2
    end
    in
    let
      module
      N
      =
      List
    in
    N.singleton M.x
  
  let x =
    let module M = struct
      let x = 2
    end
    in
    let module N =
      List
    in
    N.singleton M.x

2. let open _ in:

  $ cat > test.ml << EOF
  > open
  > M
  > 
  > let x =
  > let
  > open
  > M
  > in
  > M.x
  > 
  > let x =
  > let open
  > M
  > in
  > M.x
  > EOF

  $ ocp-indent test.ml
  open
    M
  
  let x =
    let
      open
        M
    in
    M.x
  
  let x =
    let open
      M
    in
    M.x

3. let exception _ in (inconsistent with stri):

  $ cat > test.ml << EOF
  > exception
  > E
  > of
  > int
  > 
  > let x =
  > let
  > exception
  > E
  > of
  > int
  > in
  > try 0 with E i -> i
  > 
  > let x =
  > let exception
  > E of int
  > in
  > try 0 with E i -> i
  > EOF

  $ ocp-indent test.ml
  exception
    E
    of
      int
  
  let x =
    let
      exception
      E
      of
        int
    in
    try 0 with E i -> i
  
  let x =
    let exception
      E of int
    in
    try 0 with E i -> i

4. let type t = _ in:

  $ cat > test.ml << EOF
  > type
  > t
  > =
  > | A
  > | B of int
  > 
  > let x =
  > let
  > type
  > t
  > =
  > | A
  > | B of int
  > in
  > match A with A -> 0 | B i -> i
  > 
  > let x =
  > let type t =
  > | A
  > | B of int
  > in
  > ()
  > EOF

  $ ocp-indent test.ml
  type
    t
    =
    | A
    | B of int
  
  let x =
    let
      type
        t
      =
      | A
      | B of int
    in
    match A with A -> 0 | B i -> i
  
  let x =
    let type t =
      | A
      | B of int
    in
    ()

5. let type t += _ in:

  $ cat > test.ml << EOF
  > type t
  > +=
  > | A
  > | B of int
  > 
  > let x =
  > let
  > type
  > t
  > +=
  > | A
  > | B of int
  > in
  > match A with A -> 0 | B i -> i | _ -> 0
  > 
  > let x =
  > let type t +=
  > | A
  > | B of int
  > in
  > match A with A -> 0 | B i -> i | _ -> 0
  > EOF

  $ ocp-indent test.ml
  type t
    +=
    | A
    | B of int
  
  let x =
    let
      type
        t
      +=
      | A
      | B of int
    in
    match A with A -> 0 | B i -> i | _ -> 0
  
  let x =
    let type t +=
      | A
      | B of int
    in
    match A with A -> 0 | B i -> i | _ -> 0

6. let module type _ in:

  $ cat > test.ml << EOF
  > module
  > type
  > S
  > =
  > sig
  > val x : int
  > end
  > 
  > let x =
  > let
  > module
  > type
  > S
  > =
  > sig
  > val x : int
  > end
  > in
  > ()
  > 
  > let x =
  > let module type S = sig
  > val x : int
  > end
  > in
  > ()
  > EOF

  $ ocp-indent test.ml
  module
    type
    S
  =
  sig
    val x : int
  end
  
  let x =
    let
      module
      type
      S
    =
    sig
      val x : int
    end
    in
    ()
  
  let x =
    let module type S = sig
      val x : int
    end
    in
    ()

7. let external _ in:

  $ cat > test.ml << EOF
  > external
  > f
  > :
  > int -> int
  > =
  > "f"
  > 
  > let x =
  > let
  > external
  > f
  > :
  > int -> int
  > =
  > "f"
  > in
  > f 2
  > 
  > let x =
  > let external f :
  > int -> int
  > =
  > "f"
  > in
  > f 2
  > EOF

  $ ocp-indent test.ml
  external
    f
    :
    int -> int
    =
    "f"
  
  let x =
    let
      external
        f
        :
        int -> int
      =
      "f"
    in
    f 2
  
  let x =
    let external f :
      int -> int
      =
      "f"
    in
    f 2

8. let class _ in:

  $ cat > test.ml << EOF
  > class
  > c
  > =
  > object
  > method f x = x + 1
  > end
  > 
  > let x =
  > let
  > class
  > c
  > =
  > object
  > method f x = x + 1
  > end
  > in
  > (new c)#f 1
  > 
  > let x =
  > let class c =
  > object
  > method f x = f + 1
  > end
  > in
  > (new c)#f 1
  > EOF

  $ ocp-indent test.ml
  class
    c
    =
    object
      method f x = x + 1
    end
  
  let x =
    let
      class
        c
      =
      object
        method f x = x + 1
      end
    in
    (new c)#f 1
  
  let x =
    let class c =
      object
        method f x = f + 1
      end
    in
    (new c)#f 1

9. let class type _ in:

  $ cat > test.ml << EOF
  > class
  > type
  > c
  > =
  > object
  > method f : int -> int
  > end
  > 
  > let x =
  > let
  > class
  > type
  > c
  > =
  > object
  > method f : int -> int
  > end
  > in
  > ()
  > 
  > let x =
  > let class type c =
  > object
  > method f : int -> int
  > end
  > in
  > ()
  > EOF

  $ ocp-indent test.ml
  class
    type
    c
    =
    object
      method f : int -> int
    end
  
  let x =
    let
      class
        type
        c
      =
      object
        method f : int -> int
      end
    in
    ()
  
  let x =
    let class type c =
      object
        method f : int -> int
      end
    in
    ()

9. let module rec _ in:

  $ cat > test.ml << EOF
  > module
  > rec
  > N
  > :
  > sig
  > val x : int
  > end
  > =
  > struct
  > let x = 1
  > end
  > and
  > M
  > :
  > sig
  > val x : int
  > end
  > =
  > struct
  > let x = 2
  > end
  > 
  > let x =
  > let
  > module
  > rec
  > N
  > :
  > sig
  > val x : int
  > end
  > =
  > struct
  > let x = 1
  > end
  > and
  > M
  > :
  > sig
  > val x : int
  > end
  > =
  > struct
  > let x = 2
  > end
  > in
  > M.x + N.x
  > 
  > let x =
  > let module rec N : sig
  > val x : int
  > end = struct
  > let x = 1
  > end
  > and M : sig
  > val x : int
  > end = struct
  > let x = 2
  > end
  > in
  > in
  > EOF

  $ ocp-indent test.ml
  module
    rec
    N
    :
    sig
      val x : int
    end
  =
  struct
    let x = 1
  end
  and
    M
    :
    sig
      val x : int
    end
  =
  struct
    let x = 2
  end
  
  let x =
    let
      module
      rec
      N
      :
      sig
        val x : int
      end
    =
    struct
      let x = 1
    end
    and
      M
      :
      sig
        val x : int
      end
    =
    struct
      let x = 2
    end
    in
    M.x + N.x
  
  let x =
    let module rec N : sig
      val x : int
    end = struct
      let x = 1
    end
    and M : sig
      val x : int
    end = struct
      let x = 2
    end
    in
  in
