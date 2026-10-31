open! Import
open! Enums
open! Types
open! Interfaces

(* see {!Browser.Abort_signal} for module definition *)
class type abort_signal = object
  method _any :
    signals:Interfaces.abort_signal Js.t Js.js_array Js.t
    -> Interfaces.abort_signal Js.t Js.meth

  method abort : reason:Js.Unsafe.any Js.optdef -> Interfaces.abort_signal Js.t Js.meth
  method timeout : milliseconds:Js.number Js.t -> Interfaces.abort_signal Js.t Js.meth
end

(* see {!Browser.Animation_event} for module definition *)
class type animation_event = object end

(* see {!Browser.Blob} for module definition *)
class type blob = object end

(* see {!Browser.Css_numeric_value} for module definition *)
class type css_numeric_value = object
  method parse : cssText:cssom_string Js.t -> Interfaces.css_numeric_value Js.t Js.meth
end

(* see {!Browser.Css_rule} for module definition *)
class type css_rule = object
  method _CHARSET_RULE : Js.number Js.t Js.readonly_prop
  method _COUNTER_STYLE_RULE : Js.number Js.t Js.readonly_prop
  method _FONT_FACE_RULE : Js.number Js.t Js.readonly_prop
  method _FONT_FEATURE_VALUES_RULE : Js.number Js.t Js.readonly_prop
  method _IMPORT_RULE : Js.number Js.t Js.readonly_prop
  method _KEYFRAMES_RULE : Js.number Js.t Js.readonly_prop
  method _KEYFRAME_RULE : Js.number Js.t Js.readonly_prop
  method _MEDIA_RULE : Js.number Js.t Js.readonly_prop
  method _NAMESPACE_RULE : Js.number Js.t Js.readonly_prop
  method _PAGE_RULE : Js.number Js.t Js.readonly_prop
  method _STYLE_RULE : Js.number Js.t Js.readonly_prop
  method _SUPPORTS_RULE : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Css_style_sheet} for module definition *)
class type css_style_sheet = object end

(* see {!Browser.Css_style_value} for module definition *)
class type css_style_value = object
  method parse :
    property:cssom_string Js.t
    -> cssText:cssom_string Js.t
    -> Interfaces.css_style_value Js.t Js.meth

  method parseAll :
    property:cssom_string Js.t
    -> cssText:cssom_string Js.t
    -> Interfaces.css_style_value Js.t Js.js_array Js.t Js.meth
end

(* see {!Browser.Css_unit_value} for module definition *)
class type css_unit_value = object end

(* see {!Browser.Clipboard_event} for module definition *)
class type clipboard_event = object end

(* see {!Browser.Close_event} for module definition *)
class type close_event = object end

(* see {!Browser.Comment} for module definition *)
class type comment = object end

(* see {!Browser.Compression_stream} for module definition *)
class type compression_stream = object end

(* see {!Browser.Custom_event} for module definition *)
class type custom_event = object end

(* see {!Browser.Dom_exception} for module definition *)
class type dom_exception = object
  method _ABORT_ERR : Js.number Js.t Js.readonly_prop
  method _DATA_CLONE_ERR : Js.number Js.t Js.readonly_prop
  method _DOMSTRING_SIZE_ERR : Js.number Js.t Js.readonly_prop
  method _HIERARCHY_REQUEST_ERR : Js.number Js.t Js.readonly_prop
  method _INDEX_SIZE_ERR : Js.number Js.t Js.readonly_prop
  method _INUSE_ATTRIBUTE_ERR : Js.number Js.t Js.readonly_prop
  method _INVALID_ACCESS_ERR : Js.number Js.t Js.readonly_prop
  method _INVALID_CHARACTER_ERR : Js.number Js.t Js.readonly_prop
  method _INVALID_MODIFICATION_ERR : Js.number Js.t Js.readonly_prop
  method _INVALID_NODE_TYPE_ERR : Js.number Js.t Js.readonly_prop
  method _INVALID_STATE_ERR : Js.number Js.t Js.readonly_prop
  method _NAMESPACE_ERR : Js.number Js.t Js.readonly_prop
  method _NETWORK_ERR : Js.number Js.t Js.readonly_prop
  method _NOT_FOUND_ERR : Js.number Js.t Js.readonly_prop
  method _NOT_SUPPORTED_ERR : Js.number Js.t Js.readonly_prop
  method _NO_DATA_ALLOWED_ERR : Js.number Js.t Js.readonly_prop
  method _NO_MODIFICATION_ALLOWED_ERR : Js.number Js.t Js.readonly_prop
  method _QUOTA_EXCEEDED_ERR : Js.number Js.t Js.readonly_prop
  method _SECURITY_ERR : Js.number Js.t Js.readonly_prop
  method _SYNTAX_ERR : Js.number Js.t Js.readonly_prop
  method _TIMEOUT_ERR : Js.number Js.t Js.readonly_prop
  method _TYPE_MISMATCH_ERR : Js.number Js.t Js.readonly_prop
  method _URL_MISMATCH_ERR : Js.number Js.t Js.readonly_prop
  method _VALIDATION_ERR : Js.number Js.t Js.readonly_prop
  method _WRONG_DOCUMENT_ERR : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Dom_rect} for module definition *)
class type dom_rect = object
  method fromRect :
    other:Interfaces.dom_rect_init Js.t Js.optdef -> Interfaces.dom_rect Js.t Js.meth
end

(* see {!Browser.Dom_rect_read_only} for module definition *)
class type dom_rect_read_only = object
  method fromRect :
    other:Interfaces.dom_rect_init Js.t Js.optdef
    -> Interfaces.dom_rect_read_only Js.t Js.meth
end

(* see {!Browser.Data_transfer} for module definition *)
class type data_transfer = object end

(* see {!Browser.Decompression_stream} for module definition *)
class type decompression_stream = object end

(* see {!Browser.Dedicated_worker_global_scope} for module definition *)
class type dedicated_worker_global_scope = object
  method _PERSISTENT : Js.number Js.t Js.readonly_prop
  method _TEMPORARY : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Document} for module definition *)
class type document = object
  method parseHTMLUnsafe : html:html_string Js.t -> Interfaces.document Js.t Js.meth
end

(* see {!Browser.Document_fragment} for module definition *)
class type document_fragment = object end

(* see {!Browser.Drag_event} for module definition *)
class type drag_event = object end

(* see {!Browser.Error_event} for module definition *)
class type error_event = object end

(* see {!Browser.Event} for module definition *)
class type event = object
  method _AT_TARGET : Js.number Js.t Js.readonly_prop
  method _BUBBLING_PHASE : Js.number Js.t Js.readonly_prop
  method _CAPTURING_PHASE : Js.number Js.t Js.readonly_prop
  method _NONE : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Event_target} for module definition *)
class type event_target = object end

(* see {!Browser.File} for module definition *)
class type file = object end

(* see {!Browser.File_reader} for module definition *)
class type file_reader = object
  method _DONE : Js.number Js.t Js.readonly_prop
  method _EMPTY : Js.number Js.t Js.readonly_prop
  method _LOADING : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Focus_event} for module definition *)
class type focus_event = object end

(* see {!Browser.Form_data} for module definition *)
class type form_data = object end

(* see {!Browser.Window} for module definition *)
class type geolocation_position_error = object
  method _PERMISSION_DENIED : Js.number Js.t Js.readonly_prop
  method _POSITION_UNAVAILABLE : Js.number Js.t Js.readonly_prop
  method _TIMEOUT : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Html_image_element} for module definition *)
class type html_image_element = object end

(* see {!Browser.Hash_change_event} for module definition *)
class type hash_change_event = object end

(* see {!Browser.Headers} for module definition *)
class type headers = object end

(* see {!Browser.Highlight} for module definition *)
class type highlight = object end

(* see {!Browser.Image_data} for module definition *)
class type image_data = object end

(* see {!Browser.Input_device_capabilities} for module definition *)
class type input_device_capabilities = object end

(* see {!Browser.Input_event} for module definition *)
class type input_event = object end

(* see {!Browser.Intersection_observer} for module definition *)
class type intersection_observer = object end

(* see {!Browser.Keyboard_event} for module definition *)
class type keyboard_event = object
  method _DOM_KEY_LOCATION_LEFT : Js.number Js.t Js.readonly_prop
  method _DOM_KEY_LOCATION_NUMPAD : Js.number Js.t Js.readonly_prop
  method _DOM_KEY_LOCATION_RIGHT : Js.number Js.t Js.readonly_prop
  method _DOM_KEY_LOCATION_STANDARD : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Media_source} for module definition *)
class type media_source = object
  method canConstructInDedicatedWorker : bool Js.t Js.readonly_prop
  method isTypeSupported : type_:Js.js_string Js.t -> bool Js.t Js.meth
end

(* see {!Browser.Message_event} for module definition *)
class type message_event = object end

(* see {!Browser.Mouse_event} for module definition *)
class type mouse_event = object end

(* see {!Browser.Mutation_observer} for module definition *)
class type mutation_observer = object end

(* see {!Browser.Navigate_event} for module definition *)
class type navigate_event = object end

(* see {!Browser.Node} for module definition *)
class type node = object
  method _ATTRIBUTE_NODE : Js.number Js.t Js.readonly_prop
  method _CDATA_SECTION_NODE : Js.number Js.t Js.readonly_prop
  method _COMMENT_NODE : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_FRAGMENT_NODE : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_NODE : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_CONTAINED_BY : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_CONTAINS : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_DISCONNECTED : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_FOLLOWING : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_POSITION_PRECEDING : Js.number Js.t Js.readonly_prop
  method _DOCUMENT_TYPE_NODE : Js.number Js.t Js.readonly_prop
  method _ELEMENT_NODE : Js.number Js.t Js.readonly_prop
  method _ENTITY_NODE : Js.number Js.t Js.readonly_prop
  method _ENTITY_REFERENCE_NODE : Js.number Js.t Js.readonly_prop
  method _NOTATION_NODE : Js.number Js.t Js.readonly_prop
  method _PROCESSING_INSTRUCTION_NODE : Js.number Js.t Js.readonly_prop
  method _TEXT_NODE : Js.number Js.t Js.readonly_prop
end

class type node_filter = object
  method _FILTER_ACCEPT : Js.number Js.t Js.readonly_prop
  method _FILTER_REJECT : Js.number Js.t Js.readonly_prop
  method _FILTER_SKIP : Js.number Js.t Js.readonly_prop
  method _SHOW_ALL : Js.number Js.t Js.readonly_prop
  method _SHOW_ATTRIBUTE : Js.number Js.t Js.readonly_prop
  method _SHOW_CDATA_SECTION : Js.number Js.t Js.readonly_prop
  method _SHOW_COMMENT : Js.number Js.t Js.readonly_prop
  method _SHOW_DOCUMENT : Js.number Js.t Js.readonly_prop
  method _SHOW_DOCUMENT_FRAGMENT : Js.number Js.t Js.readonly_prop
  method _SHOW_DOCUMENT_TYPE : Js.number Js.t Js.readonly_prop
  method _SHOW_ELEMENT : Js.number Js.t Js.readonly_prop
  method _SHOW_ENTITY : Js.number Js.t Js.readonly_prop
  method _SHOW_ENTITY_REFERENCE : Js.number Js.t Js.readonly_prop
  method _SHOW_NOTATION : Js.number Js.t Js.readonly_prop
  method _SHOW_PROCESSING_INSTRUCTION : Js.number Js.t Js.readonly_prop
  method _SHOW_TEXT : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Performance_observer} for module definition *)
class type performance_observer = object
  method supportedEntryTypes : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

(* see {!Browser.Pointer_event} for module definition *)
class type pointer_event = object end

(* see {!Browser.Pop_state_event} for module definition *)
class type pop_state_event = object end

(* see {!Browser.Progress_event} for module definition *)
class type progress_event = object end

(* see {!Browser.Range} for module definition *)
class type range = object
  method _END_TO_END : Js.number Js.t Js.readonly_prop
  method _END_TO_START : Js.number Js.t Js.readonly_prop
  method _START_TO_END : Js.number Js.t Js.readonly_prop
  method _START_TO_START : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Readable_stream} for module definition *)
class type readable_stream = object end

(* see {!Browser.Request} for module definition *)
class type request = object end

(* see {!Browser.Resize_observer} for module definition *)
class type resize_observer = object end

(* see {!Browser.Response} for module definition *)
class type response = object
  method error : Interfaces.response Js.t Js.meth

  method json :
    data:Js.Unsafe.any
    -> init:Interfaces.response_init Js.t Js.optdef
    -> Interfaces.response Js.t Js.meth

  method redirect :
    url:Js.js_string Js.t
    -> status:Js.number Js.t Js.optdef
    -> Interfaces.response Js.t Js.meth
end

(* see {!Browser.Shared_worker_global_scope} for module definition *)
class type shared_worker_global_scope = object
  method _PERSISTENT : Js.number Js.t Js.readonly_prop
  method _TEMPORARY : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Storage_event} for module definition *)
class type storage_event = object end

(* see {!Browser.Submit_event} for module definition *)
class type submit_event = object end

(* see {!Browser.Text} for module definition *)
class type text = object end

(* see {!Browser.Toggle_event} for module definition *)
class type toggle_event = object end

(* see {!Browser.Touch} for module definition *)
class type touch = object end

(* see {!Browser.Touch_event} for module definition *)
class type touch_event = object end

(* see {!Browser.Transition_event} for module definition *)
class type transition_event = object end

(* see {!Browser.Ui_event} for module definition *)
class type ui_event = object end

(* see {!Browser.Url} for module definition *)
class type url = object
  method canParse :
    url:Js.js_string Js.t -> base:Js.js_string Js.t Js.optdef -> bool Js.t Js.meth

  method createObjectURL : blob:Interfaces.blob Js.t -> Js.js_string Js.t Js.meth

  method createObjectURL_1 :
    source:Interfaces.media_source Js.t -> Js.js_string Js.t Js.meth

  method parse :
    url:Js.js_string Js.t
    -> base:Js.js_string Js.t Js.optdef
    -> Interfaces.url Js.t Js.opt Js.meth

  method revokeObjectURL : url:Js.js_string Js.t -> unit Js.meth
end

(* see {!Browser.Url_search_params} for module definition *)
class type url_search_params = object end

(* see {!Browser.Web_socket} for module definition *)
class type web_socket = object
  method _CLOSED : Js.number Js.t Js.readonly_prop
  method _CLOSING : Js.number Js.t Js.readonly_prop
  method _CONNECTING : Js.number Js.t Js.readonly_prop
  method _OPEN : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Wheel_event} for module definition *)
class type wheel_event = object
  method _DOM_DELTA_LINE : Js.number Js.t Js.readonly_prop
  method _DOM_DELTA_PAGE : Js.number Js.t Js.readonly_prop
  method _DOM_DELTA_PIXEL : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Window} for module definition *)
class type window = object
  method _PERSISTENT : Js.number Js.t Js.readonly_prop
  method _TEMPORARY : Js.number Js.t Js.readonly_prop
end

(* see {!Browser.Worker} for module definition *)
class type worker = object end

(* see {!Browser.Writable_stream} for module definition *)
class type writable_stream = object end

(* see {!Browser.Writable_stream_default_writer} for module definition *)
class type writable_stream_default_writer = object end

(* see {!Browser.Xml_http_request} for module definition *)
class type xml_http_request = object
  method _DONE : Js.number Js.t Js.readonly_prop
  method _HEADERS_RECEIVED : Js.number Js.t Js.readonly_prop
  method _LOADING : Js.number Js.t Js.readonly_prop
  method _OPENED : Js.number Js.t Js.readonly_prop
  method _UNSENT : Js.number Js.t Js.readonly_prop
end
