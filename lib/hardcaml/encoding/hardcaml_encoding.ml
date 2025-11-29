open! Core
open! Hardcaml
include Hardcaml_encoding_intf

module Make (M : S_encodable) : S with type t := M.t = struct
  type t = M.t [@@deriving compare ~localize, enumerate, sexp_of]

  let width = M.width

  let to_int, of_int =
    let table =
      lazy
        (let table = Int.Table.create () in
         List.iter all ~f:(fun case ->
           let encoded = M.to_int case in
           let () =
             match Bits.of_unsigned_int encoded ~width with
             | _ -> ()
             | exception _ ->
               raise_s
                 [%message
                   "Encoding doesn't fit into width"
                     (case : t)
                     (encoded : int)
                     (width : int)]
           in
           match Hashtbl.add table ~key:encoded ~data:case with
           | `Ok -> ()
           | `Duplicate ->
             let existing = Hashtbl.find_exn table encoded in
             raise_s
               [%message "Duplicate encoding" (case : t) (existing : t) (encoded : int)]);
         table)
    in
    let to_int t =
      let (_ : _) = Lazy.force table in
      M.to_int t
    in
    let of_int n = Hashtbl.find (Lazy.force table) n in
    to_int, of_int
  ;;

  let complete_ordering_exn =
    let l =
      lazy
        (let expected_options = 1 lsl width in
         let covered = List.length all in
         if covered = expected_options
         then List.init expected_options ~f:(fun i -> of_int i |> Option.value_exn)
         else
           raise_s
             [%message
               "Encoding doesn't cover all valid options"
                 (covered : int)
                 (expected_options : int)])
    in
    fun () -> Lazy.force l
  ;;

  let of_bits bits = of_int (Bits.to_unsigned_int bits)

  let valid =
    lazy
      (List.map all ~f:(fun t -> Bits.of_unsigned_int (to_int t) ~width, t)
       |> List.sort ~compare:[%compare: Bits.t * _])
  ;;

  let of_bits_or_error bits =
    match of_bits bits with
    | Some x -> Ok x
    | None ->
      let valid = Lazy.force valid in
      Or_error.error_s
        [%message
          "invalid binary representation" (bits : Bits.t) (valid : (Bits.t * t) list)]
  ;;

  let of_bits_exn bits = Or_error.ok_exn (of_bits_or_error bits)
  let to_bits t = Bits.of_unsigned_int (to_int t) ~width
  let to_signal t = Signal.of_unsigned_int (to_int t) ~width

  module Enc = struct
    type case = t
    type 'a t = { raw : 'a } [@@unboxed] [@@deriving equal ~localize, compare ~localize]

    let sexp_of_t sexp_of_a { raw } = sexp_of_a raw
    let iter t ~f = f t.raw
    let iter2 t t' ~f = f t.raw t'.raw
    let map t ~f = { raw = f t.raw }
    let map2 t t' ~f = { raw = f t.raw t'.raw }
    let to_list t = [ t.raw ]
    let port_name = "case"
    let port_names_and_widths = { raw = port_name, width }

    include functor Interface.Make

    let ast : Interface.Ast.t =
      [ { name = port_name
        ; type_ = Signal { bits = width; rtlname = port_name }
        ; sequence = None
        ; doc = None
        }
      ]
    ;;

    let to_raw t = t.raw
    let sim_set { raw } case = raw := to_bits case
    let sim_set_raw { raw } bits = raw := bits
    let sim_get { raw } = of_bits !raw
    let sim_get_or_error { raw } = of_bits_or_error !raw
    let sim_get_exn { raw } = of_bits_exn !raw
    let sim_get_raw { raw } = !raw
    let decode t = of_bits t.raw
    let decode_or_error t = of_bits_or_error t.raw
    let decode_exn t = of_bits_exn t.raw

    let of_raw (type a) (module Comb : Comb.S with type t = a) (t : a) : a t =
      let expected = width in
      let actual = Comb.width t in
      match expected = actual with
      | true -> { raw = t }
      | false -> raise_s [%message "Width mismatch" (expected : int) (actual : int)]
    ;;

    let const (type a) (module Comb : Comb.S with type t = a) case : a t =
      { raw = Comb.of_unsigned_int (to_int case) ~width }
    ;;

    let match_ (type a) (module Comb : Comb.S with type t = a) (t : a t) f : a =
      let default = f None in
      let const case = Comb.of_unsigned_int (to_int case) ~width in
      Comb.cases t.raw ~default (List.map all ~f:(fun case -> const case, f (Some case)))
    ;;

    let is_valid (type a) (module Comb : Comb.S with type t = a) (t : a t) : a =
      match_ (module Comb) t (function
        | Some _ -> Comb.vdd
        | None -> Comb.gnd)
    ;;

    let with_valid (type a) (module Comb : Comb.S with type t = a) (value : a t) =
      let valid = is_valid (module Comb) value in
      { With_valid.value; valid }
    ;;

    let of_raw_with_valid (type a) (module Comb : Comb.S with type t = a) (t : a) =
      let value = of_raw (module Comb) t in
      let valid = is_valid (module Comb) value in
      { With_valid.value; valid }
    ;;

    let is (type a) (module Comb : Comb.S with type t = a) case (t : a t) : a =
      Comb.( ==:. ) t.raw (to_int case)
    ;;

    let ( ==: ) (type a) (module Comb : Comb.S with type t = a) (t : a t) (t' : a t) : a =
      Comb.( ==: ) t.raw t'.raw
    ;;

    let mux_exn (type a) (module Comb : Comb.S with type t = a) (t : a t) (f : case -> a)
      : a
      =
      let cases = complete_ordering_exn () |> List.map ~f in
      Comb.mux t.raw cases
    ;;

    module Make_comb_fns (X : Comb.S) :
      S_enc_with_comb with type case := case and type t := X.t t and type comb := X.t =
    struct
      let const case = const (module X) case
      let of_raw raw = of_raw (module X) raw
      let of_raw_with_valid raw = of_raw_with_valid (module X) raw
      let is case t = is (module X) case t
      let is_valid t = is_valid (module X) t
      let with_valid t = with_valid (module X) t
      let ( ==: ) t t' = ( ==: ) (module X) t t'
      let match_ t f = match_ (module X) t f
      let mux_exn t f = mux_exn (module X) t f
    end

    module Make_comb (X : Comb.S) = struct
      include Make_comb (X)
      include Make_comb_fns (X)
    end

    module Of_bits = struct
      include Of_bits
      include Make_comb_fns (Bits)
    end

    module Of_signal = struct
      include Of_signal
      include Make_comb_fns (Signal)

      let reg_initial reg_spec ~initial =
        let initial_signal = const initial in
        let initial_bits = Of_bits.const initial in
        reg
          reg_spec
          initial_signal
          ~initialize_to:initial_bits
          ~clear_to:initial_signal
          ~reset_to:initial_bits
      ;;
    end

    module Of_always = struct
      include Of_always

      let set_to var case = assign var (Of_signal.const case)

      let reg_initial reg_spec ~initial =
        let initial_signal = Of_signal.const initial in
        let initial_bits = Of_bits.const initial in
        reg
          reg_spec
          ~initialize_to:initial_bits
          ~clear_to:initial_signal
          ~reset_to:initial_bits
      ;;

      let wire' x = wire (fun _ -> x.raw)

      let switch_list x f =
        let default = f None in
        let cases =
          List.filter_map all ~f:(fun case ->
            match f (Some case) with
            | [] -> None
            | ops -> Some ((Of_signal.const case).raw, ops))
        in
        let is_valid () = Of_signal.is_valid x in
        match default, cases with
        | [], [] -> []
        | [], _ :: _ -> [ Always.switch x.raw cases ]
        | _ :: _, [] -> [ Always.unless (is_valid ()) default ]
        | _ :: _, _ :: _ ->
          [ Always.if_ (is_valid ()) [ Always.switch x.raw cases ] default ]
      ;;

      let switch x f =
        match switch_list x f with
        | [ x ] -> x
        | xs -> Always.proc xs
      ;;

      let switch' x f =
        let cases = List.map all ~f:(fun case -> (Of_signal.const case).raw, f case) in
        Always.switch x.raw cases
      ;;
    end

    let of_bits = Of_bits.of_raw
    let of_signal = Of_signal.of_raw
    let bits = Of_bits.const
    let signal = Of_signal.const
    let match_bits = Of_bits.match_
    let match_signal = Of_signal.match_
    let switch_list = Of_always.switch_list
    let switch = Of_always.switch
    let switch' = Of_always.switch'
  end

  let match_bits x f = Enc.match_bits (Enc.of_bits x) f
  let match_signal x f = Enc.match_signal (Enc.of_signal x) f
  let switch_list x f = Enc.switch_list (Enc.of_signal x) f
  let switch x f = Enc.switch (Enc.of_signal x) f
  let switch' x f = Enc.switch' (Enc.of_signal x) f
end

module To_rank (M : Enum.Cases) = struct
  let to_rank =
    let f =
      lazy
        (let module Cases = struct
           type t = M.t [@@deriving compare ~localize, sexp_of]

           include functor Comparator.Make [@mode local]
         end
         in
        let map =
          List.mapi M.all ~f:(fun i x -> x, i) |> Map.of_alist_exn (module Cases)
        in
        fun n -> Map.find_exn map n)
    in
    fun n -> (Lazy.force f) n
  ;;
end

module Make_binary (M : Enum.Cases) : S with type t := M.t = struct
  include M
  include functor To_rank

  let width = Int.ceil_log2 (List.length all)
  let to_int = to_rank

  include functor Make
end

module Make_one_hot (M : Enum.Cases) : S with type t := M.t = struct
  include M
  include functor To_rank

  let width = List.length all
  let to_int t = 1 lsl to_rank t

  include functor Make
end

module Make_enum (M : Enum.Cases) : S_enums with type t := M.t = struct
  module Binary = struct
    include M
    include functor Make_binary
  end

  module One_hot = struct
    include M
    include functor Make_one_hot
  end
end
