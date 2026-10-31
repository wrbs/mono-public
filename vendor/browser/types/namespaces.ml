open! Import
open! Enums
open! Types
open! Interfaces

(* see {!Browser.Css} for module definition *)
class type css = object
  method highlights : highlight_registry Js.t Js.readonly_prop

  (** [paintWorklet] is unsafe and is only available in [Secure] contexts. In order to
      access [paintWorklet], you must use [ppx_browser]. *)
  method paintWorklet :
    ( (* unresolved Worklet *)
      untranslated Js.readonly_prop
      , [ `Secure | `Cross_origin_isolated ] )
      Browser_js_types.unsafe

  method _Hz : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method _Q : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cap : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method ch : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cm : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqb : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqmax : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqmin : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method cqw : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method deg : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dpcm : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dpi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dppx : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvb : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvmax : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvmin : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method dvw : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method em : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method escape : ident:Js.js_string Js.t -> Js.js_string Js.t Js.meth
  method ex : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method fr : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method grad : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method ic : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method in_ : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method kHz : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvb : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvmax : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvmin : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method lvw : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method mm : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method ms : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method number : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method pc : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method percent : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method pt : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method px : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method rad : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method rcap : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method rch : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method registerProperty : definition:property_definition Js.t -> unit Js.meth
  method rem : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method rex : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method ric : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method rlh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method s : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method supports : conditionText:Js.js_string Js.t -> bool Js.t Js.meth

  method supports_1 :
    property:Js.js_string Js.t -> value:Js.js_string Js.t -> bool Js.t Js.meth

  method svb : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method svh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method svi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method svmax : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method svmin : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method svw : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method turn : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vb : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vh : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vi : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vmax : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vmin : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method vw : value:Js.number Js.t -> css_unit_value Js.t Js.meth
  method x : value:Js.number Js.t -> css_unit_value Js.t Js.meth
end

(* see {!Browser.Console} for module definition *)
class type console = object
  method assert_ : condition:bool Js.t Js.optdef -> unit Js.meth
  method assert_1 : condition:bool Js.t Js.optdef -> data:Js.Unsafe.any -> unit Js.meth

  method assert_2 :
    condition:bool Js.t Js.optdef
    -> data:Js.Unsafe.any
    -> data1:Js.Unsafe.any
    -> unit Js.meth

  method assert_3 :
    condition:bool Js.t Js.optdef
    -> data:Js.Unsafe.any
    -> data1:Js.Unsafe.any
    -> data2:Js.Unsafe.any
    -> unit Js.meth

  method clear : unit Js.meth
  method count : label:Js.js_string Js.t Js.optdef -> unit Js.meth
  method countReset : label:Js.js_string Js.t Js.optdef -> unit Js.meth
  method debug : unit Js.meth
  method debug_1 : data:Js.Unsafe.any -> unit Js.meth
  method debug_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method debug_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method dir :
    item:Js.Unsafe.any Js.optdef -> options:Js.Unsafe.any Js.opt Js.optdef -> unit Js.meth

  method dirxml : unit Js.meth
  method dirxml_1 : data:Js.Unsafe.any -> unit Js.meth
  method dirxml_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method dirxml_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method error : unit Js.meth
  method error_1 : data:Js.Unsafe.any -> unit Js.meth
  method error_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method error_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method group : unit Js.meth
  method group_1 : data:Js.Unsafe.any -> unit Js.meth
  method group_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method group_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method groupCollapsed : unit Js.meth
  method groupCollapsed_1 : data:Js.Unsafe.any -> unit Js.meth
  method groupCollapsed_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method groupCollapsed_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method groupEnd : unit Js.meth
  method info : unit Js.meth
  method info_1 : data:Js.Unsafe.any -> unit Js.meth
  method info_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method info_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method log : unit Js.meth
  method log_1 : data:Js.Unsafe.any -> unit Js.meth
  method log_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method log_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method table :
    tabularData:Js.Unsafe.any Js.optdef
    -> properties:Js.js_string Js.t Js.js_array Js.t Js.optdef
    -> unit Js.meth

  method time : label:Js.js_string Js.t Js.optdef -> unit Js.meth
  method timeEnd : label:Js.js_string Js.t Js.optdef -> unit Js.meth
  method timeLog : label:Js.js_string Js.t Js.optdef -> unit Js.meth

  method timeLog_1 :
    label:Js.js_string Js.t Js.optdef -> data:Js.Unsafe.any -> unit Js.meth

  method timeLog_2 :
    label:Js.js_string Js.t Js.optdef
    -> data:Js.Unsafe.any
    -> data1:Js.Unsafe.any
    -> unit Js.meth

  method timeLog_3 :
    label:Js.js_string Js.t Js.optdef
    -> data:Js.Unsafe.any
    -> data1:Js.Unsafe.any
    -> data2:Js.Unsafe.any
    -> unit Js.meth

  method trace : unit Js.meth
  method trace_1 : data:Js.Unsafe.any -> unit Js.meth
  method trace_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method trace_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth

  method warn : unit Js.meth
  method warn_1 : data:Js.Unsafe.any -> unit Js.meth
  method warn_2 : data:Js.Unsafe.any -> data1:Js.Unsafe.any -> unit Js.meth

  method warn_3 :
    data:Js.Unsafe.any -> data1:Js.Unsafe.any -> data2:Js.Unsafe.any -> unit Js.meth
end
