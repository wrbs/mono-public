open! Core
open! Hardcaml

module type S = Validated_intf.S

module Make (C : Comb.S) : S with type base := C.t = struct
  module Validity : sig
    type t [@@deriving sexp_of, equal ~localize]

    val of_mask : C.t -> t
    val of_bit : C.t -> width:int -> t
    val to_mask : t -> C.t
    val to_bit : t -> C.t
    val ( -- ) : loc:[%call_pos] -> t -> string -> t

    (* bit/mask*)

    val kind : t -> [ `bit of C.t | `mask of C.t ]
    val has_mask : t -> bool

    (* Combinators *)

    val bit_and : t -> t -> t
    val and_ : t -> valid:C.t -> t
    val or_ : t -> valid:C.t -> t
  end = struct
    type t =
      { width : int
      ; bit : C.t
      ; mask : C.t option
      }
    [@@deriving sexp_of, equal ~localize]

    let zero_width = { width = 0; bit = C.vdd; mask = None }

    let kind t =
      match t.mask with
      | Some m -> `mask m
      | None -> `bit t.bit
    ;;

    let has_mask t = Option.is_some t.mask

    let of_bit bit ~width =
      [%test_eq: int] (C.width bit) 1;
      match width with
      | 0 -> zero_width
      | _ -> { width; bit; mask = None }
    ;;

    let to_known x = Option.try_with (fun () -> C.to_bool x)

    let of_mask mask =
      let width = C.width mask in
      match width with
      | 0 -> zero_width
      | 1 -> { width = 1; bit = mask; mask = None }
      | _ ->
        let bit = C.all_bits_set mask in
        (match to_known bit with
         | Some true -> of_bit C.vdd ~width
         | Some false -> of_bit C.gnd ~width
         | None -> { width; bit; mask = Some mask })
    ;;

    let to_bit t = t.bit

    let to_mask t =
      match t.mask with
      | None -> C.repeat t.bit ~count:t.width
      | Some mask -> mask
    ;;

    let ( -- ) ~(loc : [%call_pos]) t name =
      { width = t.width
      ; bit = C.( -- ) ~loc t.bit (name ^ "_valid")
      ; mask =
          Option.map t.mask ~f:(fun signal -> C.( -- ) ~loc signal (name ^ "_valid_mask"))
      }
    ;;

    let and_ t ~valid =
      let bit = C.( &: ) t.bit valid in
      match t.mask with
      | None -> of_bit bit ~width:t.width
      | Some m ->
        let mask = C.( &: ) m (C.repeat valid ~count:t.width) in
        { (of_mask mask) with bit }
    ;;

    let or_ t ~valid =
      let bit = C.( |: ) t.bit valid in
      match t.mask with
      | None -> of_bit bit ~width:t.width
      | Some m ->
        let mask = C.( |: ) m (C.repeat valid ~count:t.width) in
        { (of_mask mask) with bit }
    ;;

    let bit_and t t' =
      [%test_eq: int] t.width t'.width;
      let bit = C.( &: ) t.bit t'.bit in
      match t.mask, t'.mask with
      | None, None -> of_bit bit ~width:t.width
      | _ ->
        let mask = C.( &: ) (to_mask t) (to_mask t') in
        { (of_mask mask) with bit }
    ;;
  end

  type validity = Validity.t

  type t =
    { value : C.t
    ; validity : Validity.t
    }
  [@@deriving sexp_of, equal ~localize]

  let create value ~valid =
    { value; validity = Validity.of_bit valid ~width:(C.width value) }
  ;;

  let create_valid x = create x ~valid:C.vdd

  let create_with_mask value ~valid_mask =
    [%test_eq: int] (C.width value) (C.width valid_mask);
    { value; validity = Validity.of_mask valid_mask }
  ;;

  let of_with_valid ({ value; valid } : C.t With_valid.t) = create value ~valid
  let value t = t.value
  let is_valid t = Validity.to_bit t.validity
  let valid_mask t = Validity.to_mask t.validity
  let to_with_valid t : _ With_valid.t = { value = t.value; valid = is_valid t }
  let to_static_known t = Option.try_with (fun () -> C.to_bool (is_valid t))
  let is_known_valid t = [%equal: bool option] (to_static_known t) (Some true)
  let lift f t = to_with_valid t |> With_valid.map_valid ~f |> of_with_valid

  let lift2 f a b =
    With_valid.map_value2 (module C) (to_with_valid a) (to_with_valid b) ~f
    |> of_with_valid
  ;;

  let and_valid t ~valid = { t with validity = Validity.and_ t.validity ~valid }
  let or_valid t ~valid = { t with validity = Validity.or_ t.validity ~valid }

  include (
    Comb.Make (struct
      type nonrec t = t [@@deriving sexp_of, equal ~localize]

      (* Comb primitives *)

      let empty = create_valid C.empty
      let is_empty t = C.is_empty t.value
      let width t = C.width t.value
      let of_constant constant = create_valid (C.of_constant constant)
      let to_constant t = C.to_constant t.value
      let to_string t = Sexp.to_string_mach [%sexp (t : t)]

      let concat_msb = function
        | [] -> create_valid (C.concat_msb [] (* raises *))
        | [ x ] -> x
        | ts ->
          let values, masks =
            List.map ts ~f:(fun { value; validity } -> value, Validity.to_mask validity)
            |> List.unzip
          in
          create_with_mask (C.concat_msb values) ~valid_mask:(C.concat_msb masks)
      ;;

      let gnd = create_valid C.gnd
      let vdd = create_valid C.vdd

      let select t ~high ~low =
        let value = C.select t.value ~high ~low in
        match Validity.kind t.validity with
        | `bit valid -> create value ~valid
        | `mask m -> create_with_mask value ~valid_mask:(C.select m ~high ~low)
      ;;

      let ( -- ) ~(loc : [%call_pos]) t name =
        { value = C.( -- ) ~loc t.value name
        ; validity = Validity.( -- ) ~loc t.validity name
        }
      ;;

      let ( &: ) a b =
        let open C in
        let mask_a = Validity.to_mask a.validity in
        let mask_b = Validity.to_mask b.validity in
        create_with_mask
          (a.value &: b.value)
          ~valid_mask:
            (~:(a.value) &: mask_a |: (~:(b.value) &: mask_b) |: (mask_a &: mask_b))
      ;;

      let ( |: ) a b =
        let open C in
        let mask_a = Validity.to_mask a.validity in
        let mask_b = Validity.to_mask b.validity in
        create_with_mask
          (a.value |: b.value)
          ~valid_mask:(a.value &: mask_a |: (b.value &: mask_b) |: (mask_a &: mask_b))
      ;;

      let ( ^: ) a b =
        { value = C.( ^: ) a.value b.value
        ; validity = Validity.bit_and a.validity b.validity
        }
      ;;

      let ( ~: ) t = { t with value = C.( ~: ) t.value }

      let mux choice options =
        let value = C.mux choice.value (List.map options ~f:(fun t -> t.value)) in
        let base =
          match List.exists options ~f:(fun t -> Validity.has_mask t.validity) with
          | false ->
            let bits = List.map options ~f:(fun t -> Validity.to_bit t.validity) in
            create value ~valid:(C.mux choice.value bits)
          | true ->
            let masks = List.map options ~f:(fun t -> Validity.to_mask t.validity) in
            create_with_mask value ~valid_mask:(C.mux choice.value masks)
        in
        and_valid base ~valid:(is_valid choice)
      ;;

      let ( +: ) = lift2 C.( +: )
      let ( -: ) = lift2 C.( -: )
      let ( *: ) = lift2 C.( *: )
      let ( *+ ) = lift2 C.( *+ )
      let ( ==: ) = lift2 C.( ==: )
      let ( <: ) = lift2 C.( <: )

      let cases ~default selector t_cases =
        let cases =
          List.map t_cases ~f:(fun (sel, res) ->
            if is_known_valid sel
            then sel.value, res
            else
              raise_s
                [%message
                  "Validated.cases: selector values must all be valid" (sel : t) (res : t)])
        in
        let value =
          C.cases
            ~default:default.value
            selector.value
            (List.map cases ~f:(fun (sel, t) -> sel, t.value))
        in
        let case_has_mask =
          List.exists cases ~f:(fun (_, t) -> Validity.has_mask t.validity)
        in
        let base =
          match Validity.has_mask default.validity || case_has_mask with
          | false ->
            let valid_cases =
              List.map cases ~f:(fun (sel, t) -> sel, Validity.to_bit t.validity)
            in
            let valid =
              C.cases
                ~default:(Validity.to_bit default.validity)
                selector.value
                valid_cases
            in
            create value ~valid
          | true ->
            let mask_cases =
              List.map cases ~f:(fun (sel, t) -> sel, Validity.to_mask t.validity)
            in
            let valid_mask =
              C.cases
                ~default:(Validity.to_mask default.validity)
                selector.value
                mask_cases
            in
            create_with_mask value ~valid_mask
        in
        and_valid base ~valid:(is_valid selector)
      ;;
    end) :
      Comb.S with type t := t)

  let __ppx_auto_name = ( -- )
end

module Bits = Make (Bits)
include Make (Signal)
