open! Core
open! Hardcaml

module type S = Dynamic_interface_intf.S

module To_interface
    (T : S)
    (B : sig
       val bits : T.bits_param
     end) : Interface.S with type 'a t = 'a T.t = struct
  include T

  let port_names_and_widths = port_names_and_widths_dynamic ~nbits:B.bits

  include functor Interface.Make_with_wave_formats
end

module Make_of_interface'
    (Bits_param : T)
    (T : Interface.Pre)
    (Opt : sig
       val override_widths : Bits_param.t -> int option T.t
       val wave_formats : [ `Default | `Custom of Wave_format.t T.t ]
     end) : S with type 'a t = 'a T.t and type bits_param = Bits_param.t = struct
  include T

  type bits_param = Bits_param.t

  let port_names_and_widths_dynamic ~nbits =
    map2
      T.port_names_and_widths
      (Opt.override_widths nbits)
      ~f:(fun (name, orig) override -> name, Option.value override ~default:orig)
  ;;

  let wave_formats =
    match Opt.wave_formats with
    | `Default -> T.port_names_and_widths |> map ~f:(fun _ -> Wave_format.default)
    | `Custom x -> x
  ;;
end

module Of_deriving_hardcaml_pre (T : Interface.Pre) :
  S with type 'a t = 'a T.t and type bits_param = int = struct
  include T

  type bits_param = int

  let port_names_and_widths_dynamic ~nbits =
    map T.port_names_and_widths ~f:(fun (name, orig) ->
      let width = if orig < 0 then -orig * nbits else orig in
      name, width)
  ;;

  let wave_formats = T.port_names_and_widths |> map ~f:(fun _ -> Wave_format.default)
end
