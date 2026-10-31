open! Core

type t = string Quickcheck.Generator.t

let empty : t = Quickcheck.Generator.return ""
let literal s : t = Quickcheck.Generator.return s
let of_char gen : t = Quickcheck.Generator.map gen ~f:String.of_char

let concat (a : t) (b : t) : t =
  let%map.Quickcheck.Generator sa = a
  and sb = b in
  sa ^ sb
;;

let concat_list (ts : t list) : t =
  Quickcheck.Generator.all ts |> Quickcheck.Generator.map ~f:String.concat
;;

let star (g : t) : t =
  Quickcheck.Generator.recursive_union
    [ Quickcheck.Generator.return "" ]
    ~f:(fun self ->
      [ (let%map.Quickcheck.Generator s = g
         and rest = self in
         s ^ rest)
      ])
;;

let repeat ~(sep : t) ~(n : int Quickcheck.Generator.t) (item : t) : t =
  let%bind.Quickcheck.Generator n and sep in
  let%map.Quickcheck.Generator parts = Quickcheck.Generator.list_with_length n item in
  String.concat ~sep parts
;;

let choice (gs : t list) : t =
  Quickcheck.Generator.of_list gs |> Quickcheck.Generator.join
;;
