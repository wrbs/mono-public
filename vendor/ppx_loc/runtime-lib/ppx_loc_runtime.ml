open! Base

let take_last_element_in_dot_separated_string str =
  str |> String.split ~on:'.' |> List.last |> Option.value ~default:str
;;

(* A rough heuristic for determining whether a string represents a module name: check
   whether it starts with an uppercase character. *)
let looks_like_a_module_name segment =
  match String.is_empty segment with
  | true -> false
  | false -> String.get segment 0 |> Char.is_uppercase
;;

(** e.g. "Library_name__Foo.Bar" *)
let __FULL_MODULE_NAME__ ~__FUNCTION__ =
  (* [__FUNCTION__] is seemingly the only macro that contains the full module path, so we
     use it and parse the result. *)
  String.split ~on:'.' __FUNCTION__
  |> List.filter ~f:looks_like_a_module_name
  |> String.concat ~sep:"."
;;

(** e.g. if "Library_name__Foo.Bar" then "Bar" *)
let __THIS_MODULE_NAME__ ~__FUNCTION__ =
  let full_module_name = __FULL_MODULE_NAME__ ~__FUNCTION__ in
  take_last_element_in_dot_separated_string full_module_name
;;

(** same as [__THIS_MODULE_NAME__] but for functions *)
let __THIS_FUNCTION_NAME__ ~__FUNCTION__ =
  take_last_element_in_dot_separated_string __FUNCTION__
;;

let manipulate_file_name ~__FILE__ ?extension ?(filename_suffix = "") () =
  (match String.split ~on:'.' __FILE__ with
   | [ filename; original_extension ] ->
     [ filename ^ filename_suffix; Option.value extension ~default:original_extension ]
   | other -> other)
  |> String.concat ~sep:"."
;;

(** e.g. "foo.ml" *)
let __ML_FILE_NAME__ ~__FILE__ = manipulate_file_name ~__FILE__ ~extension:"ml" ()

(** e.g. "foo.mli" *)
let __MLI_FILE_NAME__ ~__FILE__ = manipulate_file_name ~__FILE__ ~extension:"mli" ()

(** e.g. "foo_intf.ml" *)
let __INTF_FILE_NAME__ ~__FILE__ =
  manipulate_file_name ~__FILE__ ~extension:"ml" ~filename_suffix:"_intf" ()
;;
