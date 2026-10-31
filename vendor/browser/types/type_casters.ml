open! Import
open! Enums
open! Types
open! Interfaces

(* TYPEDEFS *)
external blob_to_blob_part : blob Js.t -> blob_part Js.t = "%identity"

external array_buffer_view_to_blob_part
  :  Js_of_ocaml.Typed_array.arrayBufferView Js.t
  -> blob_part Js.t
  = "%identity"

external array_buffer_to_blob_part
  :  Js_of_ocaml.Typed_array.arrayBuffer Js.t
  -> blob_part Js.t
  = "%identity"

external string_to_blob_part : Js.js_string Js.t -> blob_part Js.t = "%identity"

external array_buffer_view_to_buffer_source
  :  Js_of_ocaml.Typed_array.arrayBufferView Js.t
  -> buffer_source Js.t
  = "%identity"

external array_buffer_to_buffer_source
  :  Js_of_ocaml.Typed_array.arrayBuffer Js.t
  -> buffer_source Js.t
  = "%identity"

external css_numeric_value_to_css_numberish
  :  css_numeric_value Js.t
  -> css_numberish Js.t
  = "%identity"

external float_to_css_numberish : Js.number Js.t -> css_numberish Js.t = "%identity"

external css_image_value_to_canvas_image_source
  :  (* unresolved CSSImageValue *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external html_canvas_element_to_canvas_image_source
  :  html_canvas_element Js.t
  -> canvas_image_source Js.t
  = "%identity"

external html_image_element_to_canvas_image_source
  :  html_image_element Js.t
  -> canvas_image_source Js.t
  = "%identity"

external html_video_element_to_canvas_image_source
  :  (* unresolved HTMLVideoElement *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external image_bitmap_to_canvas_image_source
  :  (* unresolved ImageBitmap *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external offscreen_canvas_to_canvas_image_source
  :  (* unresolved OffscreenCanvas *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external svg_image_element_to_canvas_image_source
  :  (* unresolved SVGImageElement *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external video_frame_to_canvas_image_source
  :  (* unresolved VideoFrame *) untranslated
  -> canvas_image_source Js.t
  = "%identity"

external constrain_boolean_parameters_to_constrain_boolean
  :  constrain_boolean_parameters Js.t
  -> constrain_boolean Js.t
  = "%identity"

external bool_to_constrain_boolean : bool Js.t -> constrain_boolean Js.t = "%identity"

external constrain_dom_string_parameters_to_constrain_dom_string
  :  constrain_dom_string_parameters Js.t
  -> constrain_dom_string Js.t
  = "%identity"

external string_array_to_constrain_dom_string
  :  Js.js_string Js.t Js.js_array Js.t
  -> constrain_dom_string Js.t
  = "%identity"

external string_to_constrain_dom_string
  :  Js.js_string Js.t
  -> constrain_dom_string Js.t
  = "%identity"

external constrain_double_range_to_constrain_double
  :  constrain_double_range Js.t
  -> constrain_double Js.t
  = "%identity"

external float_to_constrain_double : Js.number Js.t -> constrain_double Js.t = "%identity"

external constrain_long_range_to_constrain_long
  :  constrain_long_range Js.t
  -> constrain_long Js.t
  = "%identity"

external int_to_constrain_long : Js.number Js.t -> constrain_long Js.t = "%identity"

external constrain_point2d_parameters_to_constrain_point2d
  :  constrain_point2d_parameters Js.t
  -> constrain_point2d Js.t
  = "%identity"

external point2d_array_to_constrain_point2d
  :  point2d Js.t Js.js_array Js.t
  -> constrain_point2d Js.t
  = "%identity"

external file_to_form_data_entry_value
  :  file Js.t
  -> form_data_entry_value Js.t
  = "%identity"

external string_to_form_data_entry_value
  :  Js.js_string Js.t
  -> form_data_entry_value Js.t
  = "%identity"

external html_script_element_to_html_or_svg_script_element
  :  (* unresolved HTMLScriptElement *) untranslated
  -> html_or_svg_script_element Js.t
  = "%identity"

external svg_script_element_to_html_or_svg_script_element
  :  (* unresolved SVGScriptElement *) untranslated
  -> html_or_svg_script_element Js.t
  = "%identity"

external string_array_array_to_headers_init
  :  Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t
  -> headers_init Js.t
  = "%identity"

external string_record_to_headers_init
  :  Js.js_string Js.t record Js.t
  -> headers_init Js.t
  = "%identity"

external blob_to_image_bitmap_source : blob Js.t -> image_bitmap_source Js.t = "%identity"

external html_canvas_element_to_image_bitmap_source
  :  html_canvas_element Js.t
  -> image_bitmap_source Js.t
  = "%identity"

external html_image_element_to_image_bitmap_source
  :  html_image_element Js.t
  -> image_bitmap_source Js.t
  = "%identity"

external html_video_element_to_image_bitmap_source
  :  (* unresolved HTMLVideoElement *) untranslated
  -> image_bitmap_source Js.t
  = "%identity"

external image_bitmap_to_image_bitmap_source
  :  (* unresolved ImageBitmap *) untranslated
  -> image_bitmap_source Js.t
  = "%identity"

external image_data_to_image_bitmap_source
  :  image_data Js.t
  -> image_bitmap_source Js.t
  = "%identity"

external offscreen_canvas_to_image_bitmap_source
  :  (* unresolved OffscreenCanvas *) untranslated
  -> image_bitmap_source Js.t
  = "%identity"

external svg_image_element_to_image_bitmap_source
  :  (* unresolved SVGImageElement *) untranslated
  -> image_bitmap_source Js.t
  = "%identity"

external video_frame_to_image_bitmap_source
  :  (* unresolved VideoFrame *) untranslated
  -> image_bitmap_source Js.t
  = "%identity"

external dom_exception_to_media_stream_error
  :  dom_exception Js.t
  -> media_stream_error Js.t
  = "%identity"

external overconstrained_error_to_media_stream_error
  :  (* unresolved OverconstrainedError *) untranslated
  -> media_stream_error Js.t
  = "%identity"

external readable_stream_byob_reader_to_readable_stream_reader
  :  (* unresolved ReadableStreamBYOBReader *) untranslated
  -> readable_stream_reader Js.t
  = "%identity"

external readable_stream_default_reader_to_readable_stream_reader
  :  (* unresolved ReadableStreamDefaultReader *) untranslated
  -> readable_stream_reader Js.t
  = "%identity"

external canvas_rendering_context2d_to_rendering_context
  :  canvas_rendering_context2d Js.t
  -> rendering_context Js.t
  = "%identity"

external gpu_canvas_context_to_rendering_context
  :  (* unresolved GPUCanvasContext *) untranslated
  -> rendering_context Js.t
  = "%identity"

external image_bitmap_rendering_context_to_rendering_context
  :  (* unresolved ImageBitmapRenderingContext *) untranslated
  -> rendering_context Js.t
  = "%identity"

external web_gl2_rendering_context_to_rendering_context
  :  (* unresolved WebGL2RenderingContext *) untranslated
  -> rendering_context Js.t
  = "%identity"

external web_gl_rendering_context_to_rendering_context
  :  (* unresolved WebGLRenderingContext *) untranslated
  -> rendering_context Js.t
  = "%identity"

external request_to_request_info : request Js.t -> request_info Js.t = "%identity"
external string_to_request_info : Js.js_string Js.t -> request_info Js.t = "%identity"

external trusted_html_to_trusted_type
  :  (* unresolved TrustedHTML *) untranslated
  -> trusted_type Js.t
  = "%identity"

external trusted_script_to_trusted_type
  :  trusted_script Js.t
  -> trusted_type Js.t
  = "%identity"

external trusted_script_url_to_trusted_type
  :  (* unresolved TrustedScriptURL *) untranslated
  -> trusted_type Js.t
  = "%identity"

external blob_to_xml_http_request_body_init
  :  blob Js.t
  -> xml_http_request_body_init Js.t
  = "%identity"

external buffer_source_to_xml_http_request_body_init
  :  buffer_source Js.t
  -> xml_http_request_body_init Js.t
  = "%identity"

external form_data_to_xml_http_request_body_init
  :  form_data Js.t
  -> xml_http_request_body_init Js.t
  = "%identity"

external url_search_params_to_xml_http_request_body_init
  :  url_search_params Js.t
  -> xml_http_request_body_init Js.t
  = "%identity"

external string_to_xml_http_request_body_init
  :  Js.js_string Js.t
  -> xml_http_request_body_init Js.t
  = "%identity"

(* UNION TYPES *)
external add_event_listener_options_to_add_event_listener_options_or_bool
  :  add_event_listener_options Js.t
  -> add_event_listener_options_or_bool Js.t
  = "%identity"

external bool_to_add_event_listener_options_or_bool
  :  bool Js.t
  -> add_event_listener_options_or_bool Js.t
  = "%identity"

external css_numeric_value_to_css_numeric_value_or_float
  :  css_numeric_value Js.t
  -> css_numeric_value_or_float Js.t
  = "%identity"

external float_to_css_numeric_value_or_float
  :  Js.number Js.t
  -> css_numeric_value_or_float Js.t
  = "%identity"

external css_numeric_value_to_css_numeric_value_or_float_or_string
  :  css_numeric_value Js.t
  -> css_numeric_value_or_float_or_string Js.t
  = "%identity"

external float_to_css_numeric_value_or_float_or_string
  :  Js.number Js.t
  -> css_numeric_value_or_float_or_string Js.t
  = "%identity"

external string_to_css_numeric_value_or_float_or_string
  :  Js.js_string Js.t
  -> css_numeric_value_or_float_or_string Js.t
  = "%identity"

external css_style_value_to_css_style_value_or_string
  :  css_style_value Js.t
  -> css_style_value_or_string Js.t
  = "%identity"

external string_to_css_style_value_or_string
  :  Js.js_string Js.t
  -> css_style_value_or_string Js.t
  = "%identity"

external canvas_filter_to_canvas_filter_or_string
  :  (* unresolved CanvasFilter *) untranslated
  -> canvas_filter_or_string Js.t
  = "%identity"

external string_to_canvas_filter_or_string
  :  Js.js_string Js.t
  -> canvas_filter_or_string Js.t
  = "%identity"

external constrain_double_to_constrain_double_or_bool
  :  constrain_double Js.t
  -> constrain_double_or_bool Js.t
  = "%identity"

external bool_to_constrain_double_or_bool
  :  bool Js.t
  -> constrain_double_or_bool Js.t
  = "%identity"

external dom_point_init_to_dom_point_init_or_float
  :  dom_point_init Js.t
  -> dom_point_init_or_float Js.t
  = "%identity"

external float_to_dom_point_init_or_float
  :  Js.number Js.t
  -> dom_point_init_or_float Js.t
  = "%identity"

external document_to_document_or_element
  :  document Js.t
  -> document_or_element Js.t
  = "%identity"

external element_to_document_or_element
  :  element Js.t
  -> document_or_element Js.t
  = "%identity"

external document_to_document_or_xml_http_request_body_init
  :  document Js.t
  -> document_or_xml_http_request_body_init Js.t
  = "%identity"

external xml_http_request_body_init_to_document_or_xml_http_request_body_init
  :  xml_http_request_body_init Js.t
  -> document_or_xml_http_request_body_init Js.t
  = "%identity"

external element_creation_options_to_element_creation_options_or_string
  :  element_creation_options Js.t
  -> element_creation_options_or_string Js.t
  = "%identity"

external string_to_element_creation_options_or_string
  :  Js.js_string Js.t
  -> element_creation_options_or_string Js.t
  = "%identity"

external event_to_event_or_string : event Js.t -> event_or_string Js.t = "%identity"

external string_to_event_or_string
  :  Js.js_string Js.t
  -> event_or_string Js.t
  = "%identity"

external event_listener_options_to_event_listener_options_or_bool
  :  event_listener_options Js.t
  -> event_listener_options_or_bool Js.t
  = "%identity"

external bool_to_event_listener_options_or_bool
  :  bool Js.t
  -> event_listener_options_or_bool Js.t
  = "%identity"

external html_element_to_html_element_or_int
  :  html_element Js.t
  -> html_element_or_int Js.t
  = "%identity"

external int_to_html_element_or_int
  :  Js.number Js.t
  -> html_element_or_int Js.t
  = "%identity"

external html_opt_group_element_to_html_opt_group_element_or_html_option_element
  :  (* unresolved HTMLOptGroupElement *) untranslated
  -> html_opt_group_element_or_html_option_element Js.t
  = "%identity"

external html_option_element_to_html_opt_group_element_or_html_option_element
  :  (* unresolved HTMLOptionElement *) untranslated
  -> html_opt_group_element_or_html_option_element Js.t
  = "%identity"

external keyframe_animation_options_to_keyframe_animation_options_or_float
  :  keyframe_animation_options Js.t
  -> keyframe_animation_options_or_float Js.t
  = "%identity"

external float_to_keyframe_animation_options_or_float
  :  Js.number Js.t
  -> keyframe_animation_options_or_float Js.t
  = "%identity"

external media_list_to_media_list_or_string
  :  (* unresolved MediaList *) untranslated
  -> media_list_or_string Js.t
  = "%identity"

external string_to_media_list_or_string
  :  Js.js_string Js.t
  -> media_list_or_string Js.t
  = "%identity"

external media_track_constraints_to_media_track_constraints_or_bool
  :  media_track_constraints Js.t
  -> media_track_constraints_or_bool Js.t
  = "%identity"

external bool_to_media_track_constraints_or_bool
  :  bool Js.t
  -> media_track_constraints_or_bool Js.t
  = "%identity"

external node_to_node_or_trusted_script_or_string
  :  node Js.t
  -> node_or_trusted_script_or_string Js.t
  = "%identity"

external trusted_script_to_node_or_trusted_script_or_string
  :  trusted_script Js.t
  -> node_or_trusted_script_or_string Js.t
  = "%identity"

external string_to_node_or_trusted_script_or_string
  :  Js.js_string Js.t
  -> node_or_trusted_script_or_string Js.t
  = "%identity"

external readable_stream_to_readable_stream_or_xml_http_request_body_init
  :  readable_stream Js.t
  -> readable_stream_or_xml_http_request_body_init Js.t
  = "%identity"

external xml_http_request_body_init_to_readable_stream_or_xml_http_request_body_init
  :  xml_http_request_body_init Js.t
  -> readable_stream_or_xml_http_request_body_init Js.t
  = "%identity"

external scroll_into_view_options_to_scroll_into_view_options_or_bool
  :  scroll_into_view_options Js.t
  -> scroll_into_view_options_or_bool Js.t
  = "%identity"

external bool_to_scroll_into_view_options_or_bool
  :  bool Js.t
  -> scroll_into_view_options_or_bool Js.t
  = "%identity"

external timeline_range_offset_to_timeline_range_offset_or_string
  :  timeline_range_offset Js.t
  -> timeline_range_offset_or_string Js.t
  = "%identity"

external string_to_timeline_range_offset_or_string
  :  Js.js_string Js.t
  -> timeline_range_offset_or_string Js.t
  = "%identity"

external toggle_popover_options_to_toggle_popover_options_or_bool
  :  toggle_popover_options Js.t
  -> toggle_popover_options_or_bool Js.t
  = "%identity"

external bool_to_toggle_popover_options_or_bool
  :  bool Js.t
  -> toggle_popover_options_or_bool Js.t
  = "%identity"

external trusted_html_to_trusted_html_or_string
  :  (* unresolved TrustedHTML *) untranslated
  -> trusted_html_or_string Js.t
  = "%identity"

external string_to_trusted_html_or_string
  :  Js.js_string Js.t
  -> trusted_html_or_string Js.t
  = "%identity"

external trusted_script_to_trusted_script_or_string
  :  trusted_script Js.t
  -> trusted_script_or_string Js.t
  = "%identity"

external string_to_trusted_script_or_string
  :  Js.js_string Js.t
  -> trusted_script_or_string Js.t
  = "%identity"

external string_array_array_to_string_array_array_or_string_record_or_string
  :  Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t
  -> string_array_array_or_string_record_or_string Js.t
  = "%identity"

external string_record_to_string_array_array_or_string_record_or_string
  :  Js.js_string Js.t record Js.t
  -> string_array_array_or_string_record_or_string Js.t
  = "%identity"

external string_to_string_array_array_or_string_record_or_string
  :  Js.js_string Js.t
  -> string_array_array_or_string_record_or_string Js.t
  = "%identity"

external float_array_to_float_array_or_float
  :  Js.number Js.t Js.js_array Js.t
  -> float_array_or_float Js.t
  = "%identity"

external float_to_float_array_or_float
  :  Js.number Js.t
  -> float_array_or_float Js.t
  = "%identity"

external string_array_to_string_array_or_string
  :  Js.js_string Js.t Js.js_array Js.t
  -> string_array_or_string Js.t
  = "%identity"

external string_to_string_array_or_string
  :  Js.js_string Js.t
  -> string_array_or_string Js.t
  = "%identity"

external array_buffer_to_array_buffer_or_string
  :  Js_of_ocaml.Typed_array.arrayBuffer Js.t
  -> array_buffer_or_string Js.t
  = "%identity"

external string_to_array_buffer_or_string
  :  Js.js_string Js.t
  -> array_buffer_or_string Js.t
  = "%identity"

external float_to_float_or_bool_or_string
  :  Js.number Js.t
  -> float_or_bool_or_string Js.t
  = "%identity"

external bool_to_float_or_bool_or_string
  :  bool Js.t
  -> float_or_bool_or_string Js.t
  = "%identity"

external string_to_float_or_bool_or_string
  :  Js.js_string Js.t
  -> float_or_bool_or_string Js.t
  = "%identity"

(* INTERFACE ATTRIBUTE TYPES *)
external string_to_class_name_type
  :  Js.js_string Js.t
  -> class_name_type Js.t
  = "%identity"

external bool_to_return_value_type : bool Js.t -> return_value_type Js.t = "%identity"

external string_to_return_value_type
  :  Js.js_string Js.t
  -> return_value_type Js.t
  = "%identity"
