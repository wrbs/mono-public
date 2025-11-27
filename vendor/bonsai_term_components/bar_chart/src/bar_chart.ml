open! Core
open Bonsai_term

let truncate_text s max_length =
  let utf8_string = String.Utf8.of_string s in
  let total_width =
    String.Utf8.fold utf8_string ~init:0 ~f:(fun acc uchar ->
      let width = View.uchar_tty_width uchar in
      acc + width)
  in
  if total_width <= max_length
  then s
  else (
    (* Reserve space for the ellipsis character *)
    let max_width = max_length - 1 in
    let truncated, _ =
      String.Utf8.fold utf8_string ~init:([], 0) ~f:(fun (acc, current_width) uchar ->
        let char_width = View.uchar_tty_width uchar in
        if current_width + char_width <= max_width
        then uchar :: acc, current_width + char_width
        else acc, current_width)
    in
    String.Utf8.to_string (String.Utf8.of_list (List.rev truncated)) ^ "…")
;;

let get_fg_attr color = Option.map color ~f:Attr.fg |> Option.to_list
let get_bg_attr color = Option.map color ~f:Attr.bg |> Option.to_list

module Bar = struct
  type 'data t =
    { value : 'data
    ; label : string option
    ; color : Attr.Color.t option
    }
  [@@deriving fields ~getters]

  let block_eighths = [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

  let view
    (type a)
    (module M : Floatable.S with type t = a)
    ~max_bar_height
    ~(min_value : a)
    ~(max_value : a)
    ~bar_width
    ({ value; color; label = _ } : a t)
    =
    (* Shift everything down by [min_value] and then pretend [min_value] = 0 *)
    let min_value = M.to_float min_value in
    let max_value = M.to_float max_value -. min_value in
    let value = M.to_float value -. min_value in
    let units_per_cell = max_value /. Float.of_int max_bar_height in
    let height = value /. units_per_cell in
    let whole_blocks = Float.iround_exn ~dir:`Down height in
    let extra_eighths =
      (height -. Float.of_int whole_blocks) *. 8.0 |> Float.iround_exn ~dir:`Nearest
    in
    let top_block =
      if whole_blocks >= max_bar_height
      then View.none
      else
        List.init bar_width ~f:(fun _ -> block_eighths.(extra_eighths))
        |> String.concat
        |> View.text ~attrs:(get_fg_attr color)
    in
    let bar =
      (* Fill the rectangle with a character so it shows up in tests. *)
      View.vcat
        [ top_block
        ; View.vcat
          @@ List.init whole_blocks ~f:(fun _ ->
            View.text
              ~attrs:(get_fg_attr color @ get_bg_attr color)
              (String.concat (List.create ~len:bar_width "█")))
        ]
    in
    let bar =
      if View.height bar > 0
      then bar
      else View.transparent_rectangle ~width:bar_width ~height:1
    in
    View.pad bar ~t:(max_bar_height - View.height bar)
  ;;
end

module Y_range = struct
  type 'a t =
    | Constant of 'a
    | Use_most_extreme_value

  let get_y_value
    (type a)
    (module M : Floatable.S with type t = a)
    (t : a t)
    ~(bars : a Bar.t list)
    ~(get_value : float list -> compare:local_ (float -> float -> int) -> float option)
    ~(default : a)
    =
    match t with
    | Constant n -> n
    | Use_most_extreme_value ->
      List.map bars ~f:Bar.value
      |> List.map ~f:M.to_float
      |> get_value ~compare:Float.compare
      |> Option.map ~f:M.of_float
      |> Option.value ~default
  ;;

  let y_min
    (type a)
    (module M : Floatable.S with type t = a)
    (t : a t)
    (bars : a Bar.t list)
    =
    get_y_value (module M) t ~bars ~get_value:List.min_elt ~default:(M.of_float 0.0)
  ;;

  let y_max
    (type a)
    (module M : Floatable.S with type t = a)
    (t : a t)
    (bars : a Bar.t list)
    =
    get_y_value (module M) t ~bars ~get_value:List.max_elt ~default:(M.of_float 1.0)
  ;;
end

let title_view title ~max_width ~text_color ~border_color =
  (* awkward special casing for one line titles to make it so that the border box does not
     stretch the whole width if the title is skinnier than that. *)
  if String.length title <= max_width
  then
    View.text ~attrs:(get_fg_attr text_color) title
    |> Bonsai_tui_border_box.view ~attrs:(get_fg_attr border_color)
    |> View.center ~within:{ Dimensions.height = 3; width = max_width }
  else
    (* center each line rather (as opposed to centering the whole view) *)
    Bonsai_tui_typography.Text.of_string ~attr:() title
    |> List.singleton
    |> List.singleton
    |> Bonsai_tui_typography.typeset ~max_width
    |> List.map ~f:(fun line ->
      List.map line ~f:Bonsai_tui_typography.Text.to_string |> String.concat ~sep:" ")
    |> List.map ~f:(View.text ~attrs:(get_fg_attr text_color))
    |> List.map ~f:(View.center ~within:{ Dimensions.height = 1; width = max_width })
    |> View.vcat
    |> Bonsai_tui_border_box.view ~attrs:(get_fg_attr text_color)
;;

let xlabels ~label_color ~chars_per_label ~labels =
  let pad_string s =
    View.text ~attrs:(get_fg_attr label_color) s
    |> View.center ~within:{ Dimensions.height = 1; width = chars_per_label }
  in
  List.map labels ~f:(fun s -> truncate_text s (chars_per_label - 2) |> pad_string)
  |> View.hcat
;;

let ylabels
  (type a)
  (module M : Floatable.S with type t = a)
  ~label_color
  ~max_bar_height
  ~(max_value : a)
  ~(min_value : a)
  =
  let y_label_gap = 4 in
  let min_value, max_value = M.to_float min_value, M.to_float max_value in
  let units_per_cell = (max_value -. min_value) /. Float.of_int max_bar_height in
  let labels =
    List.range ~stride:(y_label_gap + 1) 0 (Int.max 1 (max_bar_height + 1))
    |> List.map ~f:(fun i -> min_value +. (units_per_cell *. Float.of_int i))
    |> List.map ~f:(Float.to_string_hum ~decimals:2)
    |> List.mapi ~f:(fun i s ->
      View.text ~attrs:(get_fg_attr label_color) s
      |> View.pad ~b:(if i = 0 then 0 else y_label_gap))
  in
  (* right justifying *)
  let longest_label_length =
    List.map labels ~f:View.width
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  List.map labels ~f:(fun label ->
    View.pad ~l:(longest_label_length - View.width label) label)
  |> List.rev
  |> View.vcat
;;

let view
  (type a)
  (module M : Floatable.S with type t = a)
  ?(theme = Theme.catpuccin ~flavor:Mocha ~data_color:Blue)
  ?(bar_padding = 2)
  ?(bar_width = 8)
  ?(show_ylabels = true)
  ?(show_xlabels = true)
  ?(title = None)
  ?(show_border = true)
  ?(y_min = Y_range.Constant (M.of_float 0.0))
  ?(y_max = Y_range.Use_most_extreme_value)
  ~max_bar_height
  (data : a Bar.t list)
  =
  let min_value, max_value =
    Y_range.y_min (module M) y_min data, Y_range.y_max (module M) y_max data
  in
  let data =
    List.map data ~f:(fun bar ->
      { bar with color = (if Option.is_some bar.color then bar.color else theme.data) })
  in
  let bars =
    match List.is_empty data with
    | true -> View.transparent_rectangle ~width:1 ~height:max_bar_height
    | false ->
      List.map
        data
        ~f:(Bar.view (module M) ~min_value ~max_value ~max_bar_height ~bar_width)
      |> List.map ~f:(View.pad ~l:bar_padding ~r:bar_padding)
      |> View.hcat
  in
  let maybe_with_border =
    if not show_border
    then bars
    else
      Bonsai_tui_border_box.view
        ~attrs:(get_fg_attr theme.border)
        ~line_type:Thick
        ~hide_right:true
        ~hide_top:true
        bars
  in
  let xlabels =
    if not show_xlabels
    then View.none
    else (
      let left_pad = Bool.to_int show_border in
      let labels = List.map data ~f:Bar.label |> List.map ~f:(Option.value ~default:"") in
      xlabels
        ~label_color:theme.label_text
        ~chars_per_label:(bar_width + (2 * bar_padding))
        ~labels
      |> View.pad ~l:left_pad)
  in
  let maybe_with_xlabels = View.vcat [ maybe_with_border; xlabels ] in
  let maybe_with_title =
    match title with
    | None -> maybe_with_xlabels
    | Some title ->
      let left_pad = Bool.to_int show_border in
      let max_width = View.width maybe_with_xlabels - left_pad - 2 in
      let title_view =
        title_view
          title
          ~max_width
          ~text_color:theme.title
          ~border_color:theme.title_border
        |> View.pad ~l:left_pad
      in
      View.vcat [ title_view; maybe_with_xlabels ]
  in
  let maybe_with_ylabels =
    if not show_ylabels
    then maybe_with_title
    else (
      (* The [View.width xlabels > 1] is to make labels line up properly for a bar chart
         with no bars. *)
      let b_pad = Bool.to_int show_border + Bool.to_int (View.width xlabels > 1) in
      let ylabels =
        ylabels
          (module M)
          ~max_bar_height
          ~min_value
          ~max_value
          ~label_color:theme.label_text
        |> View.pad ~b:b_pad
      in
      View.hcat
        [ View.pad ~t:(View.height maybe_with_title - View.height ylabels + 1) ylabels
        ; maybe_with_title
        ])
  in
  maybe_with_ylabels
;;
