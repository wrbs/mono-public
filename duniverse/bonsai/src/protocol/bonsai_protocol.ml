module Stable = struct
  open! Core.Core_stable
  open Bonsai.Stable.Private

  module Entry = struct
    module V1 = struct
      type t =
        { label : [ `Bonsai of Node_path.V1.t | `Other of string ]
        ; entry_type : string
        ; start_time : float
        ; duration : float
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 06de0862e532730a58840545d773281d |}]
      ;;
    end
  end

  module Message = struct
    module V1 = struct
      type t =
        | Graph_info of Graph_info.V1.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp, stable_variant]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b17892a0948ebd34a0b716278484df52 |}]
      ;;
    end

    module V2 = struct
      type t =
        | Graph_info of Graph_info.V2.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp, stable_variant ~version:V1.t ~modify:[ Graph_info ]]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| acf03a1188bfb7efeb8af957c2f31a09 |}]
      ;;

      let of_v1 t =
        of_V1_t t ~modify_Graph_info:(fun graph_info ->
          Graph_info (Graph_info.V2.of_v1 graph_info))
      ;;
    end

    module V3 = struct
      type t =
        | Graph_info of Graph_info.V3.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp, stable_variant ~version:V2.t ~modify:[ Graph_info ]]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b17892a0948ebd34a0b716278484df52 |}]
      ;;

      let of_v2 t =
        of_V2_t t ~modify_Graph_info:(fun graph_info ->
          Graph_info (Graph_info.V3.of_v2 graph_info))
      ;;
    end
  end

  module Worker_message = struct
    module V1 = struct
      type t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V2.t
      [@@deriving bin_io, sexp, stable_variant]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e1ff8318743ebd1c14eea6875eed5155 |}]
      ;;
    end

    module V2 = struct
      type t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V3.t
      [@@deriving bin_io, sexp, stable_variant ~version:V1.t ~modify:[ Message ]]

      let of_v1 t =
        of_V1_t t ~modify_Message:(fun message -> Message (Message.V3.of_v2 message))
      ;;

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ba4653bfa208be82da09737f35e150dc |}]
      ;;
    end
  end
end

open! Core
open! Stable
open Bonsai.Private

module Versioned_message = struct
  type t =
    | V1 of Message.V1.t list
    | V2 of Message.V2.t list
    | V3 of Worker_message.V1.t list
    | V4 of Worker_message.V2.t list
  [@@deriving sexp, bin_io]
end

module Entry = struct
  type t = Entry.V1.t =
    { label : [ `Bonsai of Node_path.t | `Other of string ]
    ; entry_type : string
    ; start_time : float
    ; duration : float
    }
  [@@deriving bin_io, sexp]
end

module Message = struct
  type t = Message.V3.t =
    | Graph_info of Graph_info.t
    | Performance_measure of Entry.t
  [@@deriving bin_io, sexp]
end

module Worker_message = struct
  type t = Worker_message.V2.t =
    | Uuid of Uuid.Unstable.t
    | Message of Message.t
end
