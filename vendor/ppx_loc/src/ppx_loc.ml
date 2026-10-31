open! Base
open! Import

let extension ~name ~f =
  Extension.declare
    [%string "loc.%{name}"]
    Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> f ~loc)
;;

let module_name =
  extension ~name:"module_name" ~f:(fun ~loc ->
    [%expr (Ppx_loc_runtime.__FULL_MODULE_NAME__ ~__FUNCTION__ : string) [@merlin.hide]])
;;

let toplevel_module_name =
  extension ~name:"toplevel_module_name" ~f:(fun ~loc ->
    [%expr (__MODULE__ : string) [@merlin.hide]])
;;

let this_module_name =
  extension ~name:"this_module_name" ~f:(fun ~loc ->
    [%expr (Ppx_loc_runtime.__THIS_MODULE_NAME__ ~__FUNCTION__ : string) [@merlin.hide]])
;;

let ml_file_name =
  extension ~name:"ml_file_name" ~f:(fun ~loc ->
    [%expr (Ppx_loc_runtime.__ML_FILE_NAME__ ~__FILE__ : string) [@merlin.hide]])
;;

let mli_file_name =
  extension ~name:"mli_file_name" ~f:(fun ~loc ->
    [%expr (Ppx_loc_runtime.__MLI_FILE_NAME__ ~__FILE__ : string) [@merlin.hide]])
;;

let intf_file_name =
  extension ~name:"intf_file_name" ~f:(fun ~loc ->
    [%expr (Ppx_loc_runtime.__INTF_FILE_NAME__ ~__FILE__ : string) [@merlin.hide]])
;;

let file_name =
  extension ~name:"file_name" ~f:(fun ~loc -> [%expr (__FILE__ : string) [@merlin.hide]])
;;

let pos =
  extension ~name:"pos" ~f:(fun ~loc ->
    [%expr (__POS__ : string * int * int * int) [@merlin.hide]])
;;

let function_name =
  extension ~name:"function_name" ~f:(fun ~loc ->
    [%expr (__FUNCTION__ : string) [@merlin.hide]])
;;

let this_function_name =
  extension ~name:"this_function_name" ~f:(fun ~loc ->
    [%expr
      (Ppx_loc_runtime.__THIS_FUNCTION_NAME__ ~__FUNCTION__:Stdlib.__FUNCTION__
       : string)
       [@merlin.hide]])
;;

let line_number =
  extension ~name:"line_number" ~f:(fun ~loc -> [%expr (__LINE__ : int) [@merlin.hide]])
;;

let () =
  Driver.register_transformation
    "ppx_loc"
    ~extensions:
      [ module_name
      ; toplevel_module_name
      ; this_module_name
      ; ml_file_name
      ; mli_file_name
      ; intf_file_name
      ; file_name
      ; pos
      ; function_name
      ; this_function_name
      ; line_number
      ]
;;
