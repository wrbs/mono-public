open! Import
open! Enums
open! Types
open! Namespaces

class type dedicated_worker_global_scope = object
  inherit Interfaces.dedicated_worker_global_scope
  method _AbortSignal : Class_definitions.abort_signal Js.t Js.prop

  method _AbortSignal_t :
    Interfaces.abort_signal Browser_js_types.instance_t Js.readonly_prop

  method _Blob : Class_definitions.blob Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Blob_constructor :
    (blobParts:blob_part Js.t Js.js_array Js.t Js.optdef
     -> options:Interfaces.blob_property_bag Js.t Js.optdef
     -> Interfaces.blob Js.t)
      Js.constr
      Js.readonly_prop

  method _Blob_t : Interfaces.blob Browser_js_types.instance_t Js.readonly_prop

  method _CanvasGradient_t :
    Interfaces.canvas_gradient Browser_js_types.instance_t Js.readonly_prop

  method _CanvasPattern_t :
    Interfaces.canvas_pattern Browser_js_types.instance_t Js.readonly_prop

  method _CloseEvent : Class_definitions.close_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CloseEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.close_event_init Js.t Js.optdef
     -> Interfaces.close_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CloseEvent_t :
    Interfaces.close_event Browser_js_types.instance_t Js.readonly_prop

  method _CompressionStream : Class_definitions.compression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.compression_stream Js.t) Js.constr
      Js.readonly_prop

  method _CompressionStream_t :
    Interfaces.compression_stream Browser_js_types.instance_t Js.readonly_prop

  method _CustomEvent : Class_definitions.custom_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CustomEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.custom_event_init Js.t Js.optdef
     -> Interfaces.custom_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CustomEvent_t :
    Interfaces.custom_event Browser_js_types.instance_t Js.readonly_prop

  method _DOMException : Class_definitions.dom_exception Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMException_constructor :
    (message:Js.js_string Js.t Js.optdef
     -> name:Js.js_string Js.t Js.optdef
     -> Interfaces.dom_exception Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMException_t :
    Interfaces.dom_exception Browser_js_types.instance_t Js.readonly_prop

  method _DOMRect : Class_definitions.dom_rect Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRect_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRect_t : Interfaces.dom_rect Browser_js_types.instance_t Js.readonly_prop
  method _DOMRectReadOnly : Class_definitions.dom_rect_read_only Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRectReadOnly_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect_read_only Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRectReadOnly_t :
    Interfaces.dom_rect_read_only Browser_js_types.instance_t Js.readonly_prop

  method _DecompressionStream : Class_definitions.decompression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DecompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.decompression_stream Js.t) Js.constr
      Js.readonly_prop

  method _DecompressionStream_t :
    Interfaces.decompression_stream Browser_js_types.instance_t Js.readonly_prop

  method _DedicatedWorkerGlobalScope :
    Class_definitions.dedicated_worker_global_scope Js.t Js.prop

  method _DedicatedWorkerGlobalScope_t :
    Interfaces.dedicated_worker_global_scope Browser_js_types.instance_t Js.readonly_prop

  method _ErrorEvent : Class_definitions.error_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ErrorEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.error_event_init Js.t Js.optdef
     -> Interfaces.error_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ErrorEvent_t :
    Interfaces.error_event Browser_js_types.instance_t Js.readonly_prop

  method _Event : Class_definitions.event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Event_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.event_init Js.t Js.optdef
     -> Interfaces.event Js.t)
      Js.constr
      Js.readonly_prop

  method _Event_t : Interfaces.event Browser_js_types.instance_t Js.readonly_prop
  method _EventTarget : Class_definitions.event_target Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _EventTarget_constructor :
    Interfaces.event_target Js.t Js.constr Js.readonly_prop

  method _EventTarget_t :
    Interfaces.event_target Browser_js_types.instance_t Js.readonly_prop

  method _File : Class_definitions.file Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _File_constructor :
    (fileBits:blob_part Js.t Js.js_array Js.t
     -> fileName:Js.js_string Js.t
     -> options:Interfaces.file_property_bag Js.t Js.optdef
     -> Interfaces.file Js.t)
      Js.constr
      Js.readonly_prop

  method _File_t : Interfaces.file Browser_js_types.instance_t Js.readonly_prop
  method _FileList_t : Interfaces.file_list Browser_js_types.instance_t Js.readonly_prop
  method _FileReader : Class_definitions.file_reader Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FileReader_constructor : Interfaces.file_reader Js.t Js.constr Js.readonly_prop

  method _FileReader_t :
    Interfaces.file_reader Browser_js_types.instance_t Js.readonly_prop

  method _FormData : Class_definitions.form_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FormData_constructor :
    (form:(* unresolved HTMLFormElement *) untranslated Js.optdef
     -> submitter:Interfaces.html_element Js.t Js.opt Js.optdef
     -> Interfaces.form_data Js.t)
      Js.constr
      Js.readonly_prop

  method _FormData_t : Interfaces.form_data Browser_js_types.instance_t Js.readonly_prop
  method _Headers : Class_definitions.headers Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Headers_constructor :
    (init:headers_init Js.t Js.optdef -> Interfaces.headers Js.t) Js.constr
      Js.readonly_prop

  method _Headers_t : Interfaces.headers Browser_js_types.instance_t Js.readonly_prop
  method _ImageData : Class_definitions.image_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor :
    (sw:Js.number Js.t -> sh:Js.number Js.t -> Interfaces.image_data Js.t) Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor1 :
    (data:Js_of_ocaml.Typed_array.uint16Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor2 :
    (data:(* Uint8ClampedArray *) untranslated
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor3 :
    (data:Js_of_ocaml.Typed_array.float32Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor4 :
    (sw:Js.number Js.t
     -> sh:Js.number Js.t
     -> settings:Interfaces.image_data_settings Js.t
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  method _ImageData_t : Interfaces.image_data Browser_js_types.instance_t Js.readonly_prop
  method _MediaSource : Class_definitions.media_source Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MediaSource_constructor :
    Interfaces.media_source Js.t Js.constr Js.readonly_prop

  method _MediaSource_t :
    Interfaces.media_source Browser_js_types.instance_t Js.readonly_prop

  method _MessageEvent : Class_definitions.message_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MessageEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.message_event_init Js.t Js.optdef
     -> Interfaces.message_event Js.t)
      Js.constr
      Js.readonly_prop

  method _MessageEvent_t :
    Interfaces.message_event Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceEntry_t :
    Interfaces.performance_entry Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceObserver : Class_definitions.performance_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _PerformanceObserver_constructor :
    (callback:
       ( unit
         , entries:(* unresolved PerformanceObserverEntryList *) untranslated
           -> observer:Interfaces.performance_observer Js.t
           -> options:Interfaces.performance_observer_callback_options Js.t Js.optdef
           -> unit )
         Js.meth_callback
     -> Interfaces.performance_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _PerformanceObserver_t :
    Interfaces.performance_observer Browser_js_types.instance_t Js.readonly_prop

  method _ProgressEvent : Class_definitions.progress_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ProgressEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.progress_event_init Js.t Js.optdef
     -> Interfaces.progress_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ProgressEvent_t :
    Interfaces.progress_event Browser_js_types.instance_t Js.readonly_prop

  method _ReadableStream : Class_definitions.readable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ReadableStream_constructor :
    (underlyingSource:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.readable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _ReadableStream_t :
    Interfaces.readable_stream Browser_js_types.instance_t Js.readonly_prop

  method _Request : Class_definitions.request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Request_constructor :
    (input:request_info Js.t
     -> init:Interfaces.request_init Js.t Js.optdef
     -> Interfaces.request Js.t)
      Js.constr
      Js.readonly_prop

  method _Request_t : Interfaces.request Browser_js_types.instance_t Js.readonly_prop
  method _Response : Class_definitions.response Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Response_constructor :
    (body:Js.Unsafe.any Js.optdef
     -> init:Interfaces.response_init Js.t Js.optdef
     -> Interfaces.response Js.t)
      Js.constr
      Js.readonly_prop

  method _Response_t : Interfaces.response Browser_js_types.instance_t Js.readonly_prop

  method _TextMetrics_t :
    Interfaces.text_metrics Browser_js_types.instance_t Js.readonly_prop

  method _TrustedScript_t :
    Interfaces.trusted_script Browser_js_types.instance_t Js.readonly_prop

  method _URL : Class_definitions.url Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URL_constructor :
    (url:Js.js_string Js.t -> base:Js.js_string Js.t Js.optdef -> Interfaces.url Js.t)
      Js.constr
      Js.readonly_prop

  method _URL_t : Interfaces.url Browser_js_types.instance_t Js.readonly_prop
  method _URLSearchParams : Class_definitions.url_search_params Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URLSearchParams_constructor :
    (init:string_array_array_or_string_record_or_string Js.t Js.optdef
     -> Interfaces.url_search_params Js.t)
      Js.constr
      Js.readonly_prop

  method _URLSearchParams_t :
    Interfaces.url_search_params Browser_js_types.instance_t Js.readonly_prop

  method _WebSocket : Class_definitions.web_socket Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WebSocket_constructor :
    (url:Js.js_string Js.t
     -> protocols:string_array_or_string Js.t Js.optdef
     -> Interfaces.web_socket Js.t)
      Js.constr
      Js.readonly_prop

  method _WebSocket_t : Interfaces.web_socket Browser_js_types.instance_t Js.readonly_prop
  method _Worker : Class_definitions.worker Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Worker_constructor :
    (scriptURL:script_url_string Js.t
     -> options:Interfaces.worker_options Js.t Js.optdef
     -> Interfaces.worker Js.t)
      Js.constr
      Js.readonly_prop

  method _Worker_t : Interfaces.worker Browser_js_types.instance_t Js.readonly_prop

  method _WorkerGlobalScope_t :
    Interfaces.worker_global_scope Browser_js_types.instance_t Js.readonly_prop

  method _WritableStream : Class_definitions.writable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStream_constructor :
    (underlyingSink:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.writable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStream_t :
    Interfaces.writable_stream Browser_js_types.instance_t Js.readonly_prop

  method _WritableStreamDefaultWriter :
    Class_definitions.writable_stream_default_writer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStreamDefaultWriter_constructor :
    (stream:Interfaces.writable_stream Js.t
     -> Interfaces.writable_stream_default_writer Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStreamDefaultWriter_t :
    Interfaces.writable_stream_default_writer Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequest : Class_definitions.xml_http_request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _XMLHttpRequest_constructor :
    Interfaces.xml_http_request Js.t Js.constr Js.readonly_prop

  method _XMLHttpRequest_t :
    Interfaces.xml_http_request Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestEventTarget_t :
    Interfaces.xml_http_request_event_target Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestUpload_t :
    Interfaces.xml_http_request_upload Browser_js_types.instance_t Js.readonly_prop

  method console : console Js.t Js.prop
end

class type shared_worker_global_scope = object
  inherit Interfaces.shared_worker_global_scope
  method _AbortSignal : Class_definitions.abort_signal Js.t Js.prop

  method _AbortSignal_t :
    Interfaces.abort_signal Browser_js_types.instance_t Js.readonly_prop

  method _Blob : Class_definitions.blob Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Blob_constructor :
    (blobParts:blob_part Js.t Js.js_array Js.t Js.optdef
     -> options:Interfaces.blob_property_bag Js.t Js.optdef
     -> Interfaces.blob Js.t)
      Js.constr
      Js.readonly_prop

  method _Blob_t : Interfaces.blob Browser_js_types.instance_t Js.readonly_prop

  method _CanvasGradient_t :
    Interfaces.canvas_gradient Browser_js_types.instance_t Js.readonly_prop

  method _CanvasPattern_t :
    Interfaces.canvas_pattern Browser_js_types.instance_t Js.readonly_prop

  method _CloseEvent : Class_definitions.close_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CloseEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.close_event_init Js.t Js.optdef
     -> Interfaces.close_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CloseEvent_t :
    Interfaces.close_event Browser_js_types.instance_t Js.readonly_prop

  method _CompressionStream : Class_definitions.compression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.compression_stream Js.t) Js.constr
      Js.readonly_prop

  method _CompressionStream_t :
    Interfaces.compression_stream Browser_js_types.instance_t Js.readonly_prop

  method _CustomEvent : Class_definitions.custom_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CustomEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.custom_event_init Js.t Js.optdef
     -> Interfaces.custom_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CustomEvent_t :
    Interfaces.custom_event Browser_js_types.instance_t Js.readonly_prop

  method _DOMException : Class_definitions.dom_exception Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMException_constructor :
    (message:Js.js_string Js.t Js.optdef
     -> name:Js.js_string Js.t Js.optdef
     -> Interfaces.dom_exception Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMException_t :
    Interfaces.dom_exception Browser_js_types.instance_t Js.readonly_prop

  method _DOMRect : Class_definitions.dom_rect Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRect_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRect_t : Interfaces.dom_rect Browser_js_types.instance_t Js.readonly_prop
  method _DOMRectReadOnly : Class_definitions.dom_rect_read_only Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRectReadOnly_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect_read_only Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRectReadOnly_t :
    Interfaces.dom_rect_read_only Browser_js_types.instance_t Js.readonly_prop

  method _DecompressionStream : Class_definitions.decompression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DecompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.decompression_stream Js.t) Js.constr
      Js.readonly_prop

  method _DecompressionStream_t :
    Interfaces.decompression_stream Browser_js_types.instance_t Js.readonly_prop

  method _ErrorEvent : Class_definitions.error_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ErrorEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.error_event_init Js.t Js.optdef
     -> Interfaces.error_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ErrorEvent_t :
    Interfaces.error_event Browser_js_types.instance_t Js.readonly_prop

  method _Event : Class_definitions.event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Event_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.event_init Js.t Js.optdef
     -> Interfaces.event Js.t)
      Js.constr
      Js.readonly_prop

  method _Event_t : Interfaces.event Browser_js_types.instance_t Js.readonly_prop
  method _EventTarget : Class_definitions.event_target Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _EventTarget_constructor :
    Interfaces.event_target Js.t Js.constr Js.readonly_prop

  method _EventTarget_t :
    Interfaces.event_target Browser_js_types.instance_t Js.readonly_prop

  method _File : Class_definitions.file Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _File_constructor :
    (fileBits:blob_part Js.t Js.js_array Js.t
     -> fileName:Js.js_string Js.t
     -> options:Interfaces.file_property_bag Js.t Js.optdef
     -> Interfaces.file Js.t)
      Js.constr
      Js.readonly_prop

  method _File_t : Interfaces.file Browser_js_types.instance_t Js.readonly_prop
  method _FileList_t : Interfaces.file_list Browser_js_types.instance_t Js.readonly_prop
  method _FileReader : Class_definitions.file_reader Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FileReader_constructor : Interfaces.file_reader Js.t Js.constr Js.readonly_prop

  method _FileReader_t :
    Interfaces.file_reader Browser_js_types.instance_t Js.readonly_prop

  method _FormData : Class_definitions.form_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FormData_constructor :
    (form:(* unresolved HTMLFormElement *) untranslated Js.optdef
     -> submitter:Interfaces.html_element Js.t Js.opt Js.optdef
     -> Interfaces.form_data Js.t)
      Js.constr
      Js.readonly_prop

  method _FormData_t : Interfaces.form_data Browser_js_types.instance_t Js.readonly_prop
  method _Headers : Class_definitions.headers Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Headers_constructor :
    (init:headers_init Js.t Js.optdef -> Interfaces.headers Js.t) Js.constr
      Js.readonly_prop

  method _Headers_t : Interfaces.headers Browser_js_types.instance_t Js.readonly_prop
  method _ImageData : Class_definitions.image_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor :
    (sw:Js.number Js.t -> sh:Js.number Js.t -> Interfaces.image_data Js.t) Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor1 :
    (data:Js_of_ocaml.Typed_array.uint16Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor2 :
    (data:(* Uint8ClampedArray *) untranslated
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor3 :
    (data:Js_of_ocaml.Typed_array.float32Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor4 :
    (sw:Js.number Js.t
     -> sh:Js.number Js.t
     -> settings:Interfaces.image_data_settings Js.t
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  method _ImageData_t : Interfaces.image_data Browser_js_types.instance_t Js.readonly_prop
  method _MessageEvent : Class_definitions.message_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MessageEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.message_event_init Js.t Js.optdef
     -> Interfaces.message_event Js.t)
      Js.constr
      Js.readonly_prop

  method _MessageEvent_t :
    Interfaces.message_event Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceEntry_t :
    Interfaces.performance_entry Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceObserver : Class_definitions.performance_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _PerformanceObserver_constructor :
    (callback:
       ( unit
         , entries:(* unresolved PerformanceObserverEntryList *) untranslated
           -> observer:Interfaces.performance_observer Js.t
           -> options:Interfaces.performance_observer_callback_options Js.t Js.optdef
           -> unit )
         Js.meth_callback
     -> Interfaces.performance_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _PerformanceObserver_t :
    Interfaces.performance_observer Browser_js_types.instance_t Js.readonly_prop

  method _ProgressEvent : Class_definitions.progress_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ProgressEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.progress_event_init Js.t Js.optdef
     -> Interfaces.progress_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ProgressEvent_t :
    Interfaces.progress_event Browser_js_types.instance_t Js.readonly_prop

  method _ReadableStream : Class_definitions.readable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ReadableStream_constructor :
    (underlyingSource:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.readable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _ReadableStream_t :
    Interfaces.readable_stream Browser_js_types.instance_t Js.readonly_prop

  method _Request : Class_definitions.request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Request_constructor :
    (input:request_info Js.t
     -> init:Interfaces.request_init Js.t Js.optdef
     -> Interfaces.request Js.t)
      Js.constr
      Js.readonly_prop

  method _Request_t : Interfaces.request Browser_js_types.instance_t Js.readonly_prop
  method _Response : Class_definitions.response Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Response_constructor :
    (body:Js.Unsafe.any Js.optdef
     -> init:Interfaces.response_init Js.t Js.optdef
     -> Interfaces.response Js.t)
      Js.constr
      Js.readonly_prop

  method _Response_t : Interfaces.response Browser_js_types.instance_t Js.readonly_prop

  method _SharedWorkerGlobalScope :
    Class_definitions.shared_worker_global_scope Js.t Js.prop

  method _SharedWorkerGlobalScope_t :
    Interfaces.shared_worker_global_scope Browser_js_types.instance_t Js.readonly_prop

  method _TextMetrics_t :
    Interfaces.text_metrics Browser_js_types.instance_t Js.readonly_prop

  method _TrustedScript_t :
    Interfaces.trusted_script Browser_js_types.instance_t Js.readonly_prop

  method _URL : Class_definitions.url Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URL_constructor :
    (url:Js.js_string Js.t -> base:Js.js_string Js.t Js.optdef -> Interfaces.url Js.t)
      Js.constr
      Js.readonly_prop

  method _URL_t : Interfaces.url Browser_js_types.instance_t Js.readonly_prop
  method _URLSearchParams : Class_definitions.url_search_params Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URLSearchParams_constructor :
    (init:string_array_array_or_string_record_or_string Js.t Js.optdef
     -> Interfaces.url_search_params Js.t)
      Js.constr
      Js.readonly_prop

  method _URLSearchParams_t :
    Interfaces.url_search_params Browser_js_types.instance_t Js.readonly_prop

  method _WebSocket : Class_definitions.web_socket Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WebSocket_constructor :
    (url:Js.js_string Js.t
     -> protocols:string_array_or_string Js.t Js.optdef
     -> Interfaces.web_socket Js.t)
      Js.constr
      Js.readonly_prop

  method _WebSocket_t : Interfaces.web_socket Browser_js_types.instance_t Js.readonly_prop

  method _WorkerGlobalScope_t :
    Interfaces.worker_global_scope Browser_js_types.instance_t Js.readonly_prop

  method _WritableStream : Class_definitions.writable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStream_constructor :
    (underlyingSink:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.writable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStream_t :
    Interfaces.writable_stream Browser_js_types.instance_t Js.readonly_prop

  method _WritableStreamDefaultWriter :
    Class_definitions.writable_stream_default_writer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStreamDefaultWriter_constructor :
    (stream:Interfaces.writable_stream Js.t
     -> Interfaces.writable_stream_default_writer Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStreamDefaultWriter_t :
    Interfaces.writable_stream_default_writer Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequest : Class_definitions.xml_http_request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _XMLHttpRequest_constructor :
    Interfaces.xml_http_request Js.t Js.constr Js.readonly_prop

  method _XMLHttpRequest_t :
    Interfaces.xml_http_request Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestEventTarget_t :
    Interfaces.xml_http_request_event_target Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestUpload_t :
    Interfaces.xml_http_request_upload Browser_js_types.instance_t Js.readonly_prop

  method console : console Js.t Js.prop
end

class type window = object
  inherit Interfaces.window
  method _AbortSignal : Class_definitions.abort_signal Js.t Js.prop

  method _AbortSignal_t :
    Interfaces.abort_signal Browser_js_types.instance_t Js.readonly_prop

  method _AbstractRange_t :
    Interfaces.abstract_range Browser_js_types.instance_t Js.readonly_prop

  method _AnimationEvent : Class_definitions.animation_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _AnimationEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.animation_event_init Js.t Js.optdef
     -> Interfaces.animation_event Js.t)
      Js.constr
      Js.readonly_prop

  method _AnimationEvent_t :
    Interfaces.animation_event Browser_js_types.instance_t Js.readonly_prop

  method _Attr_t : Interfaces.attr Browser_js_types.instance_t Js.readonly_prop

  method _BeforeUnloadEvent_t :
    Interfaces.before_unload_event Browser_js_types.instance_t Js.readonly_prop

  method _Blob : Class_definitions.blob Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Blob_constructor :
    (blobParts:blob_part Js.t Js.js_array Js.t Js.optdef
     -> options:Interfaces.blob_property_bag Js.t Js.optdef
     -> Interfaces.blob Js.t)
      Js.constr
      Js.readonly_prop

  method _Blob_t : Interfaces.blob Browser_js_types.instance_t Js.readonly_prop
  method _CSS : css Js.t Js.prop
  method _CSSNumericValue : Class_definitions.css_numeric_value Js.t Js.prop

  method _CSSNumericValue_t :
    Interfaces.css_numeric_value Browser_js_types.instance_t Js.readonly_prop

  method _CSSRule : Class_definitions.css_rule Js.t Js.prop
  method _CSSRule_t : Interfaces.css_rule Browser_js_types.instance_t Js.readonly_prop

  method _CSSStyleDeclaration_t :
    Interfaces.css_style_declaration Browser_js_types.instance_t Js.readonly_prop

  method _CSSStyleSheet : Class_definitions.css_style_sheet Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CSSStyleSheet_constructor :
    (options:Interfaces.css_style_sheet_init Js.t Js.optdef
     -> Interfaces.css_style_sheet Js.t)
      Js.constr
      Js.readonly_prop

  method _CSSStyleSheet_t :
    Interfaces.css_style_sheet Browser_js_types.instance_t Js.readonly_prop

  method _CSSStyleValue : Class_definitions.css_style_value Js.t Js.prop

  method _CSSStyleValue_t :
    Interfaces.css_style_value Browser_js_types.instance_t Js.readonly_prop

  method _CSSUnitValue : Class_definitions.css_unit_value Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CSSUnitValue_constructor :
    (value:Js.number Js.t -> unit:cssom_string Js.t -> Interfaces.css_unit_value Js.t)
      Js.constr
      Js.readonly_prop

  method _CSSUnitValue_t :
    Interfaces.css_unit_value Browser_js_types.instance_t Js.readonly_prop

  method _CanvasGradient_t :
    Interfaces.canvas_gradient Browser_js_types.instance_t Js.readonly_prop

  method _CanvasPattern_t :
    Interfaces.canvas_pattern Browser_js_types.instance_t Js.readonly_prop

  method _CanvasRenderingContext2D_t :
    Interfaces.canvas_rendering_context2d Browser_js_types.instance_t Js.readonly_prop

  method _CharacterData_t :
    Interfaces.character_data Browser_js_types.instance_t Js.readonly_prop

  method _ClipboardEvent : Class_definitions.clipboard_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ClipboardEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.clipboard_event_init Js.t Js.optdef
     -> Interfaces.clipboard_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ClipboardEvent_t :
    Interfaces.clipboard_event Browser_js_types.instance_t Js.readonly_prop

  method _CloseEvent : Class_definitions.close_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CloseEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.close_event_init Js.t Js.optdef
     -> Interfaces.close_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CloseEvent_t :
    Interfaces.close_event Browser_js_types.instance_t Js.readonly_prop

  method _Comment : Class_definitions.comment Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Comment_constructor :
    (data:Js.js_string Js.t Js.optdef -> Interfaces.comment Js.t) Js.constr
      Js.readonly_prop

  method _Comment_t : Interfaces.comment Browser_js_types.instance_t Js.readonly_prop
  method _CompressionStream : Class_definitions.compression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.compression_stream Js.t) Js.constr
      Js.readonly_prop

  method _CompressionStream_t :
    Interfaces.compression_stream Browser_js_types.instance_t Js.readonly_prop

  method _CustomEvent : Class_definitions.custom_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _CustomEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.custom_event_init Js.t Js.optdef
     -> Interfaces.custom_event Js.t)
      Js.constr
      Js.readonly_prop

  method _CustomEvent_t :
    Interfaces.custom_event Browser_js_types.instance_t Js.readonly_prop

  method _DOMException : Class_definitions.dom_exception Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMException_constructor :
    (message:Js.js_string Js.t Js.optdef
     -> name:Js.js_string Js.t Js.optdef
     -> Interfaces.dom_exception Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMException_t :
    Interfaces.dom_exception Browser_js_types.instance_t Js.readonly_prop

  method _DOMImplementation_t :
    Interfaces.dom_implementation Browser_js_types.instance_t Js.readonly_prop

  method _DOMRect : Class_definitions.dom_rect Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRect_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRect_t : Interfaces.dom_rect Browser_js_types.instance_t Js.readonly_prop

  method _DOMRectList_t :
    Interfaces.dom_rect_list Browser_js_types.instance_t Js.readonly_prop

  method _DOMRectReadOnly : Class_definitions.dom_rect_read_only Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DOMRectReadOnly_constructor :
    (x:Js.number Js.t Js.optdef
     -> y:Js.number Js.t Js.optdef
     -> width:Js.number Js.t Js.optdef
     -> height:Js.number Js.t Js.optdef
     -> Interfaces.dom_rect_read_only Js.t)
      Js.constr
      Js.readonly_prop

  method _DOMRectReadOnly_t :
    Interfaces.dom_rect_read_only Browser_js_types.instance_t Js.readonly_prop

  method _DOMTokenList_t :
    Interfaces.dom_token_list Browser_js_types.instance_t Js.readonly_prop

  method _DataTransfer : Class_definitions.data_transfer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DataTransfer_constructor :
    Interfaces.data_transfer Js.t Js.constr Js.readonly_prop

  method _DataTransfer_t :
    Interfaces.data_transfer Browser_js_types.instance_t Js.readonly_prop

  method _DataTransferItem_t :
    Interfaces.data_transfer_item Browser_js_types.instance_t Js.readonly_prop

  method _DataTransferItemList_t :
    Interfaces.data_transfer_item_list Browser_js_types.instance_t Js.readonly_prop

  method _DecompressionStream : Class_definitions.decompression_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DecompressionStream_constructor :
    (format:Js.js_string Js.t -> Interfaces.decompression_stream Js.t) Js.constr
      Js.readonly_prop

  method _DecompressionStream_t :
    Interfaces.decompression_stream Browser_js_types.instance_t Js.readonly_prop

  method _Document : Class_definitions.document Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Document_constructor : Interfaces.document Js.t Js.constr Js.readonly_prop

  method _Document_t : Interfaces.document Browser_js_types.instance_t Js.readonly_prop
  method _DocumentFragment : Class_definitions.document_fragment Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DocumentFragment_constructor :
    Interfaces.document_fragment Js.t Js.constr Js.readonly_prop

  method _DocumentFragment_t :
    Interfaces.document_fragment Browser_js_types.instance_t Js.readonly_prop

  method _DocumentType_t :
    Interfaces.document_type Browser_js_types.instance_t Js.readonly_prop

  method _DragEvent : Class_definitions.drag_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _DragEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.drag_event_init Js.t Js.optdef
     -> Interfaces.drag_event Js.t)
      Js.constr
      Js.readonly_prop

  method _DragEvent_t : Interfaces.drag_event Browser_js_types.instance_t Js.readonly_prop
  method _Element_t : Interfaces.element Browser_js_types.instance_t Js.readonly_prop
  method _ErrorEvent : Class_definitions.error_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ErrorEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.error_event_init Js.t Js.optdef
     -> Interfaces.error_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ErrorEvent_t :
    Interfaces.error_event Browser_js_types.instance_t Js.readonly_prop

  method _Event : Class_definitions.event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Event_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.event_init Js.t Js.optdef
     -> Interfaces.event Js.t)
      Js.constr
      Js.readonly_prop

  method _Event_t : Interfaces.event Browser_js_types.instance_t Js.readonly_prop
  method _EventTarget : Class_definitions.event_target Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _EventTarget_constructor :
    Interfaces.event_target Js.t Js.constr Js.readonly_prop

  method _EventTarget_t :
    Interfaces.event_target Browser_js_types.instance_t Js.readonly_prop

  method _File : Class_definitions.file Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _File_constructor :
    (fileBits:blob_part Js.t Js.js_array Js.t
     -> fileName:Js.js_string Js.t
     -> options:Interfaces.file_property_bag Js.t Js.optdef
     -> Interfaces.file Js.t)
      Js.constr
      Js.readonly_prop

  method _File_t : Interfaces.file Browser_js_types.instance_t Js.readonly_prop
  method _FileList_t : Interfaces.file_list Browser_js_types.instance_t Js.readonly_prop
  method _FileReader : Class_definitions.file_reader Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FileReader_constructor : Interfaces.file_reader Js.t Js.constr Js.readonly_prop

  method _FileReader_t :
    Interfaces.file_reader Browser_js_types.instance_t Js.readonly_prop

  method _FocusEvent : Class_definitions.focus_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FocusEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.focus_event_init Js.t Js.optdef
     -> Interfaces.focus_event Js.t)
      Js.constr
      Js.readonly_prop

  method _FocusEvent_t :
    Interfaces.focus_event Browser_js_types.instance_t Js.readonly_prop

  method _FormData : Class_definitions.form_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _FormData_constructor :
    (form:(* unresolved HTMLFormElement *) untranslated Js.optdef
     -> submitter:Interfaces.html_element Js.t Js.opt Js.optdef
     -> Interfaces.form_data Js.t)
      Js.constr
      Js.readonly_prop

  method _FormData_t : Interfaces.form_data Browser_js_types.instance_t Js.readonly_prop

  method _Geolocation_t :
    Interfaces.geolocation Browser_js_types.instance_t Js.readonly_prop

  method _GeolocationPosition_t :
    Interfaces.geolocation_position Browser_js_types.instance_t Js.readonly_prop

  method _GeolocationPositionError :
    Class_definitions.geolocation_position_error Js.t Js.prop

  method _GeolocationPositionError_t :
    Interfaces.geolocation_position_error Browser_js_types.instance_t Js.readonly_prop

  method _HTMLAnchorElement_t :
    Interfaces.html_anchor_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLBodyElement_t :
    Interfaces.html_body_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLButtonElement_t :
    Interfaces.html_button_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLCanvasElement_t :
    Interfaces.html_canvas_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLCollection_t :
    Interfaces.html_collection Browser_js_types.instance_t Js.readonly_prop

  method _HTMLDivElement_t :
    Interfaces.html_div_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLElement_t :
    Interfaces.html_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLIFrameElement_t :
    Interfaces.html_iframe_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLImageElement : Class_definitions.html_image_element Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _HTMLImageElement_constructor :
    Interfaces.html_image_element Js.t Js.constr Js.readonly_prop

  method _HTMLImageElement_t :
    Interfaces.html_image_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLInputElement_t :
    Interfaces.html_input_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLMetaElement_t :
    Interfaces.html_meta_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLSelectElement_t :
    Interfaces.html_select_element Browser_js_types.instance_t Js.readonly_prop

  method _HTMLTextAreaElement_t :
    Interfaces.html_text_area_element Browser_js_types.instance_t Js.readonly_prop

  method _HashChangeEvent : Class_definitions.hash_change_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _HashChangeEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.hash_change_event_init Js.t Js.optdef
     -> Interfaces.hash_change_event Js.t)
      Js.constr
      Js.readonly_prop

  method _HashChangeEvent_t :
    Interfaces.hash_change_event Browser_js_types.instance_t Js.readonly_prop

  method _Headers : Class_definitions.headers Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Headers_constructor :
    (init:headers_init Js.t Js.optdef -> Interfaces.headers Js.t) Js.constr
      Js.readonly_prop

  method _Headers_t : Interfaces.headers Browser_js_types.instance_t Js.readonly_prop
  method _Highlight : Class_definitions.highlight Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Highlight_constructor :
    (initRanges:Interfaces.abstract_range Js.t Js.js_array Js.t
     -> Interfaces.highlight Js.t)
      Js.constr
      Js.readonly_prop

  method _Highlight_t : Interfaces.highlight Browser_js_types.instance_t Js.readonly_prop

  method _HighlightRegistry_t :
    Interfaces.highlight_registry Browser_js_types.instance_t Js.readonly_prop

  method _History_t : Interfaces.history Browser_js_types.instance_t Js.readonly_prop
  method _ImageData : Class_definitions.image_data Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor :
    (sw:Js.number Js.t -> sh:Js.number Js.t -> Interfaces.image_data Js.t) Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor1 :
    (data:Js_of_ocaml.Typed_array.uint16Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor2 :
    (data:(* Uint8ClampedArray *) untranslated
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor3 :
    (data:Js_of_ocaml.Typed_array.float32Array Js.t
     -> sw:Js.number Js.t
     -> sh:Js.number Js.t Js.optdef
     -> settings:Interfaces.image_data_settings Js.t Js.optdef
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ImageData_constructor4 :
    (sw:Js.number Js.t
     -> sh:Js.number Js.t
     -> settings:Interfaces.image_data_settings Js.t
     -> Interfaces.image_data Js.t)
      Js.constr
      Js.readonly_prop

  method _ImageData_t : Interfaces.image_data Browser_js_types.instance_t Js.readonly_prop

  method _InputDeviceCapabilities :
    Class_definitions.input_device_capabilities Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _InputDeviceCapabilities_constructor :
    (deviceInitDict:Interfaces.input_device_capabilities_init Js.t Js.optdef
     -> Interfaces.input_device_capabilities Js.t)
      Js.constr
      Js.readonly_prop

  method _InputDeviceCapabilities_t :
    Interfaces.input_device_capabilities Browser_js_types.instance_t Js.readonly_prop

  method _InputEvent : Class_definitions.input_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _InputEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.input_event_init Js.t Js.optdef
     -> Interfaces.input_event Js.t)
      Js.constr
      Js.readonly_prop

  method _InputEvent_t :
    Interfaces.input_event Browser_js_types.instance_t Js.readonly_prop

  method _IntersectionObserver : Class_definitions.intersection_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _IntersectionObserver_constructor :
    (callback:
       ( unit
         , entries:Interfaces.intersection_observer_entry Js.t Js.js_array Js.t
           -> observer:Interfaces.intersection_observer Js.t
           -> unit )
         Js.meth_callback
     -> options:Interfaces.intersection_observer_init Js.t Js.optdef
     -> Interfaces.intersection_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _IntersectionObserver_t :
    Interfaces.intersection_observer Browser_js_types.instance_t Js.readonly_prop

  method _IntersectionObserverEntry_t :
    Interfaces.intersection_observer_entry Browser_js_types.instance_t Js.readonly_prop

  method _KeyboardEvent : Class_definitions.keyboard_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _KeyboardEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.keyboard_event_init Js.t Js.optdef
     -> Interfaces.keyboard_event Js.t)
      Js.constr
      Js.readonly_prop

  method _KeyboardEvent_t :
    Interfaces.keyboard_event Browser_js_types.instance_t Js.readonly_prop

  method _Location_t : Interfaces.location Browser_js_types.instance_t Js.readonly_prop

  method _MediaQueryList_t :
    Interfaces.media_query_list Browser_js_types.instance_t Js.readonly_prop

  method _MediaSource : Class_definitions.media_source Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MediaSource_constructor :
    Interfaces.media_source Js.t Js.constr Js.readonly_prop

  method _MediaSource_t :
    Interfaces.media_source Browser_js_types.instance_t Js.readonly_prop

  method _MessageEvent : Class_definitions.message_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MessageEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.message_event_init Js.t Js.optdef
     -> Interfaces.message_event Js.t)
      Js.constr
      Js.readonly_prop

  method _MessageEvent_t :
    Interfaces.message_event Browser_js_types.instance_t Js.readonly_prop

  method _MouseEvent : Class_definitions.mouse_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MouseEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.mouse_event_init Js.t Js.optdef
     -> Interfaces.mouse_event Js.t)
      Js.constr
      Js.readonly_prop

  method _MouseEvent_t :
    Interfaces.mouse_event Browser_js_types.instance_t Js.readonly_prop

  method _MutationObserver : Class_definitions.mutation_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _MutationObserver_constructor :
    (callback:
       ( unit
         , mutations:Interfaces.mutation_record Js.t Js.js_array Js.t
           -> observer:Interfaces.mutation_observer Js.t
           -> unit )
         Js.meth_callback
     -> Interfaces.mutation_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _MutationObserver_t :
    Interfaces.mutation_observer Browser_js_types.instance_t Js.readonly_prop

  method _MutationRecord_t :
    Interfaces.mutation_record Browser_js_types.instance_t Js.readonly_prop

  method _NamedNodeMap_t :
    Interfaces.named_node_map Browser_js_types.instance_t Js.readonly_prop

  method _NavigateEvent : Class_definitions.navigate_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _NavigateEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInit:Interfaces.navigate_event_init Js.t
     -> Interfaces.navigate_event Js.t)
      Js.constr
      Js.readonly_prop

  method _NavigateEvent_t :
    Interfaces.navigate_event Browser_js_types.instance_t Js.readonly_prop

  method _Navigation_t :
    Interfaces.navigation Browser_js_types.instance_t Js.readonly_prop

  method _NavigationHistoryEntry_t :
    Interfaces.navigation_history_entry Browser_js_types.instance_t Js.readonly_prop

  method _Navigator_t : Interfaces.navigator Browser_js_types.instance_t Js.readonly_prop
  method _Node : Class_definitions.node Js.t Js.prop
  method _Node_t : Interfaces.node Browser_js_types.instance_t Js.readonly_prop
  method _NodeFilter : Class_definitions.node_filter Js.t Js.prop

  method _NodeIterator_t :
    Interfaces.node_iterator Browser_js_types.instance_t Js.readonly_prop

  method _NodeList_t : Interfaces.node_list Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceEntry_t :
    Interfaces.performance_entry Browser_js_types.instance_t Js.readonly_prop

  method _PerformanceObserver : Class_definitions.performance_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _PerformanceObserver_constructor :
    (callback:
       ( unit
         , entries:(* unresolved PerformanceObserverEntryList *) untranslated
           -> observer:Interfaces.performance_observer Js.t
           -> options:Interfaces.performance_observer_callback_options Js.t Js.optdef
           -> unit )
         Js.meth_callback
     -> Interfaces.performance_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _PerformanceObserver_t :
    Interfaces.performance_observer Browser_js_types.instance_t Js.readonly_prop

  method _PointerEvent : Class_definitions.pointer_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _PointerEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.pointer_event_init Js.t Js.optdef
     -> Interfaces.pointer_event Js.t)
      Js.constr
      Js.readonly_prop

  method _PointerEvent_t :
    Interfaces.pointer_event Browser_js_types.instance_t Js.readonly_prop

  method _PopStateEvent : Class_definitions.pop_state_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _PopStateEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.pop_state_event_init Js.t Js.optdef
     -> Interfaces.pop_state_event Js.t)
      Js.constr
      Js.readonly_prop

  method _PopStateEvent_t :
    Interfaces.pop_state_event Browser_js_types.instance_t Js.readonly_prop

  method _ProgressEvent : Class_definitions.progress_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ProgressEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.progress_event_init Js.t Js.optdef
     -> Interfaces.progress_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ProgressEvent_t :
    Interfaces.progress_event Browser_js_types.instance_t Js.readonly_prop

  method _RadioNodeList_t :
    Interfaces.radio_node_list Browser_js_types.instance_t Js.readonly_prop

  method _Range : Class_definitions.range Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Range_constructor : Interfaces.range Js.t Js.constr Js.readonly_prop

  method _Range_t : Interfaces.range Browser_js_types.instance_t Js.readonly_prop
  method _ReadableStream : Class_definitions.readable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ReadableStream_constructor :
    (underlyingSource:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.readable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _ReadableStream_t :
    Interfaces.readable_stream Browser_js_types.instance_t Js.readonly_prop

  method _Request : Class_definitions.request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Request_constructor :
    (input:request_info Js.t
     -> init:Interfaces.request_init Js.t Js.optdef
     -> Interfaces.request Js.t)
      Js.constr
      Js.readonly_prop

  method _Request_t : Interfaces.request Browser_js_types.instance_t Js.readonly_prop
  method _ResizeObserver : Class_definitions.resize_observer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ResizeObserver_constructor :
    (callback:
       ( unit
         , entries:Interfaces.resize_observer_entry Js.t Js.js_array Js.t
           -> observer:Interfaces.resize_observer Js.t
           -> unit )
         Js.meth_callback
     -> Interfaces.resize_observer Js.t)
      Js.constr
      Js.readonly_prop

  method _ResizeObserver_t :
    Interfaces.resize_observer Browser_js_types.instance_t Js.readonly_prop

  method _ResizeObserverEntry_t :
    Interfaces.resize_observer_entry Browser_js_types.instance_t Js.readonly_prop

  method _ResizeObserverSize_t :
    Interfaces.resize_observer_size Browser_js_types.instance_t Js.readonly_prop

  method _Response : Class_definitions.response Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Response_constructor :
    (body:Js.Unsafe.any Js.optdef
     -> init:Interfaces.response_init Js.t Js.optdef
     -> Interfaces.response Js.t)
      Js.constr
      Js.readonly_prop

  method _Response_t : Interfaces.response Browser_js_types.instance_t Js.readonly_prop
  method _Screen_t : Interfaces.screen Browser_js_types.instance_t Js.readonly_prop
  method _Selection_t : Interfaces.selection Browser_js_types.instance_t Js.readonly_prop
  method _Storage_t : Interfaces.storage Browser_js_types.instance_t Js.readonly_prop
  method _StorageEvent : Class_definitions.storage_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _StorageEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.storage_event_init Js.t Js.optdef
     -> Interfaces.storage_event Js.t)
      Js.constr
      Js.readonly_prop

  method _StorageEvent_t :
    Interfaces.storage_event Browser_js_types.instance_t Js.readonly_prop

  method _StylePropertyMap_t :
    Interfaces.style_property_map Browser_js_types.instance_t Js.readonly_prop

  method _StylePropertyMapReadOnly_t :
    Interfaces.style_property_map_read_only Browser_js_types.instance_t Js.readonly_prop

  method _StyleSheet_t :
    Interfaces.style_sheet Browser_js_types.instance_t Js.readonly_prop

  method _SubmitEvent : Class_definitions.submit_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _SubmitEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.submit_event_init Js.t Js.optdef
     -> Interfaces.submit_event Js.t)
      Js.constr
      Js.readonly_prop

  method _SubmitEvent_t :
    Interfaces.submit_event Browser_js_types.instance_t Js.readonly_prop

  method _Text : Class_definitions.text Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Text_constructor :
    (data:Js.js_string Js.t Js.optdef -> Interfaces.text Js.t) Js.constr Js.readonly_prop

  method _Text_t : Interfaces.text Browser_js_types.instance_t Js.readonly_prop

  method _TextMetrics_t :
    Interfaces.text_metrics Browser_js_types.instance_t Js.readonly_prop

  method _ToggleEvent : Class_definitions.toggle_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _ToggleEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.toggle_event_init Js.t Js.optdef
     -> Interfaces.toggle_event Js.t)
      Js.constr
      Js.readonly_prop

  method _ToggleEvent_t :
    Interfaces.toggle_event Browser_js_types.instance_t Js.readonly_prop

  method _Touch : Class_definitions.touch Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Touch_constructor :
    (initDict:Interfaces.touch_init Js.t -> Interfaces.touch Js.t) Js.constr
      Js.readonly_prop

  method _Touch_t : Interfaces.touch Browser_js_types.instance_t Js.readonly_prop
  method _TouchEvent : Class_definitions.touch_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _TouchEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.touch_event_init Js.t Js.optdef
     -> Interfaces.touch_event Js.t)
      Js.constr
      Js.readonly_prop

  method _TouchEvent_t :
    Interfaces.touch_event Browser_js_types.instance_t Js.readonly_prop

  method _TouchList_t : Interfaces.touch_list Browser_js_types.instance_t Js.readonly_prop
  method _TransitionEvent : Class_definitions.transition_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _TransitionEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.transition_event_init Js.t Js.optdef
     -> Interfaces.transition_event Js.t)
      Js.constr
      Js.readonly_prop

  method _TransitionEvent_t :
    Interfaces.transition_event Browser_js_types.instance_t Js.readonly_prop

  method _TrustedScript_t :
    Interfaces.trusted_script Browser_js_types.instance_t Js.readonly_prop

  method _UIEvent : Class_definitions.ui_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _UIEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.ui_event_init Js.t Js.optdef
     -> Interfaces.ui_event Js.t)
      Js.constr
      Js.readonly_prop

  method _UIEvent_t : Interfaces.ui_event Browser_js_types.instance_t Js.readonly_prop
  method _URL : Class_definitions.url Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URL_constructor :
    (url:Js.js_string Js.t -> base:Js.js_string Js.t Js.optdef -> Interfaces.url Js.t)
      Js.constr
      Js.readonly_prop

  method _URL_t : Interfaces.url Browser_js_types.instance_t Js.readonly_prop
  method _URLSearchParams : Class_definitions.url_search_params Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _URLSearchParams_constructor :
    (init:string_array_array_or_string_record_or_string Js.t Js.optdef
     -> Interfaces.url_search_params Js.t)
      Js.constr
      Js.readonly_prop

  method _URLSearchParams_t :
    Interfaces.url_search_params Browser_js_types.instance_t Js.readonly_prop

  method _WebSocket : Class_definitions.web_socket Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WebSocket_constructor :
    (url:Js.js_string Js.t
     -> protocols:string_array_or_string Js.t Js.optdef
     -> Interfaces.web_socket Js.t)
      Js.constr
      Js.readonly_prop

  method _WebSocket_t : Interfaces.web_socket Browser_js_types.instance_t Js.readonly_prop
  method _WheelEvent : Class_definitions.wheel_event Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WheelEvent_constructor :
    (type_:Js.js_string Js.t
     -> eventInitDict:Interfaces.wheel_event_init Js.t Js.optdef
     -> Interfaces.wheel_event Js.t)
      Js.constr
      Js.readonly_prop

  method _WheelEvent_t :
    Interfaces.wheel_event Browser_js_types.instance_t Js.readonly_prop

  method _Window : Class_definitions.window Js.t Js.prop
  method _Window_t : Interfaces.window Browser_js_types.instance_t Js.readonly_prop
  method _Worker : Class_definitions.worker Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _Worker_constructor :
    (scriptURL:script_url_string Js.t
     -> options:Interfaces.worker_options Js.t Js.optdef
     -> Interfaces.worker Js.t)
      Js.constr
      Js.readonly_prop

  method _Worker_t : Interfaces.worker Browser_js_types.instance_t Js.readonly_prop
  method _WritableStream : Class_definitions.writable_stream Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStream_constructor :
    (underlyingSink:Js.Unsafe.any Js.optdef
     -> strategy:Js.Unsafe.any Js.optdef
     -> Interfaces.writable_stream Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStream_t :
    Interfaces.writable_stream Browser_js_types.instance_t Js.readonly_prop

  method _WritableStreamDefaultWriter :
    Class_definitions.writable_stream_default_writer Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _WritableStreamDefaultWriter_constructor :
    (stream:Interfaces.writable_stream Js.t
     -> Interfaces.writable_stream_default_writer Js.t)
      Js.constr
      Js.readonly_prop

  method _WritableStreamDefaultWriter_t :
    Interfaces.writable_stream_default_writer Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequest : Class_definitions.xml_http_request Js.t Js.prop

  (** Constructors must be bound to an identifier before being called with [new%js] *)
  method _XMLHttpRequest_constructor :
    Interfaces.xml_http_request Js.t Js.constr Js.readonly_prop

  method _XMLHttpRequest_t :
    Interfaces.xml_http_request Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestEventTarget_t :
    Interfaces.xml_http_request_event_target Browser_js_types.instance_t Js.readonly_prop

  method _XMLHttpRequestUpload_t :
    Interfaces.xml_http_request_upload Browser_js_types.instance_t Js.readonly_prop

  method console : console Js.t Js.prop
end
