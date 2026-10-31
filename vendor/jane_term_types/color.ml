open Core

module M = struct
  type term =
    [ `Term_black
    | `Term_blue
    | `Term_brown
    | `Term_yellow
      (* `Brown and `Yellow map to the same underlying color, which shows up as brown or
         yellow, depending on the terminal. An xterm by default emulates an old terminal
         that was not capable of pale yellow foreground or yellow background. So, ordinary
         `Brown|`Yellow shows up as brown, but "bold" shows up as yellow. Background shows
         up as brown. If you really want yellow background, you can run the xterm and tell
         it to emulate a more capable terminal, e.g. with:

         xterm -tn xterm-256color
      *)
    | `Term_cyan
    | `Term_green
    | `Term_magenta
    | `Term_red
    | `Term_white
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  type web =
    [ (* http://en.wikipedia.org/wiki/Web_colors#X11_color_names *)
      `Maroon
    | `Dark_red
    | `Brown
    | `Fire_brick
    | `Crimson
    | `Red
    | `Tomato
    | `Coral
    | `Indian_red
    | `Light_coral
    | `Dark_salmon
    | `Salmon
    | `Light_salmon
    | `Orange_red
    | `Dark_orange
    | `Orange
    | `Gold
    | `Dark_golden_rod
    | `Golden_rod
    | `Pale_golden_rod
    | `Dark_khaki
    | `Khaki
    | `Olive
    | `Yellow
    | `Yellow_green
    | `Dark_olive_green
    | `Olive_drab
    | `Lawn_green
    | `Chart_reuse
    | `Green_yellow
    | `Dark_green
    | `Green
    | `Forest_green
    | `Lime
    | `Lime_green
    | `Light_green
    | `Pale_green
    | `Dark_sea_green
    | `Medium_spring_green
    | `Spring_green
    | `Sea_green
    | `Medium_aqua_marine
    | `Medium_sea_green
    | `Light_sea_green
    | `Dark_slate_gray
    | `Teal
    | `Dark_cyan
    | `Cyan
    | `Light_cyan
    | `Dark_turquoise
    | `Turquoise
    | `Medium_turquoise
    | `Pale_turquoise
    | `Aqua_marine
    | `Powder_blue
    | `Cadet_blue
    | `Steel_blue
    | `Corn_flowerblue
    | `Deep_skyblue
    | `Dodger_blue
    | `Light_blue
    | `Skyblue
    | `Light_sky_blue
    | `Midnight_blue
    | `Navy
    | `Dark_blue
    | `Medium_blue
    | `Blue
    | `Royal_blue
    | `Blue_violet
    | `Indigo
    | `Dark_slate_blue
    | `Slate_blue
    | `Medium_slate_blue
    | `Medium_purple
    | `Dark_magenta
    | `Dark_violet
    | `Dark_orchid
    | `Medium_orchid
    | `Purple
    | `Thistle
    | `Plum
    | `Violet
    | `Magenta
    | `Fuchsia
    | `Orchid
    | `Medium_violet_red
    | `Pale_violet_red
    | `Deep_pink
    | `Hot_pink
    | `Light_pink
    | `Pink
    | `Antique_white
    | `Beige
    | `Bisque
    | `Blanched_almond
    | `Wheat
    | `Corn_silk
    | `Lemon_chiffon
    | `Light_golden_rod_yellow
    | `Light_yellow
    | `Saddle_brown
    | `Sienna
    | `Chocolate
    | `Peru
    | `Sandy_brown
    | `Burly_wood
    | `Tan
    | `Rosy_brown
    | `Moccasin
    | `Navajo_white
    | `Peach_puff
    | `Misty_rose
    | `Lavender_blush
    | `Linen
    | `Old_lace
    | `Papaya_whip
    | `Seashell
    | `Mint_cream
    | `Slate_gray
    | `Light_slate_gray
    | `Light_steel_blue
    | `Lavender
    | `Floral_white
    | `Alice_blue
    | `Ghost_white
    | `Honeydew
    | `Ivory
    | `Azure
    | `Snow
    | `Black
    | `Dim_gray
    | `Gray
    | `Dark_gray
    | `Silver
    | `Light_gray
    | `Gainsboro
    | `White_smoke
    | `White
    | `Gray10
    | `Gray20
    | `Gray30
    | `Gray40
    | `Gray50
    | `Gray60
    | `Gray70
    | `Gray80
    | `Gray90
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  type t =
    [ term
    | web
    | `RGB of int * int * int
    ]
  [@@deriving sexp, bin_io, compare ~localize]

  let rgb_of_web web =
    match web with
    | `Maroon -> 128, 0, 0
    | `Dark_red -> 139, 0, 0
    | `Brown -> 165, 42, 42
    | `Fire_brick -> 178, 34, 34
    | `Crimson -> 220, 20, 60
    | `Red -> 255, 0, 0
    | `Tomato -> 255, 99, 71
    | `Coral -> 255, 127, 80
    | `Indian_red -> 205, 92, 92
    | `Light_coral -> 240, 128, 128
    | `Dark_salmon -> 233, 150, 122
    | `Salmon -> 250, 128, 114
    | `Light_salmon -> 255, 160, 122
    | `Orange_red -> 255, 69, 0
    | `Dark_orange -> 255, 140, 0
    | `Orange -> 255, 165, 0
    | `Gold -> 255, 215, 0
    | `Dark_golden_rod -> 184, 134, 11
    | `Golden_rod -> 218, 165, 32
    | `Pale_golden_rod -> 238, 232, 170
    | `Dark_khaki -> 189, 183, 107
    | `Khaki -> 240, 230, 140
    | `Olive -> 128, 128, 0
    | `Yellow -> 255, 255, 0
    | `Yellow_green -> 154, 205, 50
    | `Dark_olive_green -> 85, 107, 47
    | `Olive_drab -> 107, 142, 35
    | `Lawn_green -> 124, 252, 0
    | `Chart_reuse -> 127, 255, 0
    | `Green_yellow -> 173, 255, 47
    | `Dark_green -> 0, 100, 0
    | `Green -> 0, 128, 0
    | `Forest_green -> 34, 139, 34
    | `Lime -> 0, 255, 0
    | `Lime_green -> 50, 205, 50
    | `Light_green -> 144, 238, 144
    | `Pale_green -> 152, 251, 152
    | `Dark_sea_green -> 143, 188, 143
    | `Medium_spring_green -> 0, 250, 154
    | `Spring_green -> 0, 255, 127
    | `Sea_green -> 46, 139, 87
    | `Medium_aqua_marine -> 102, 205, 170
    | `Medium_sea_green -> 60, 179, 113
    | `Light_sea_green -> 32, 178, 170
    | `Dark_slate_gray -> 47, 79, 79
    | `Teal -> 0, 128, 128
    | `Dark_cyan -> 0, 139, 139
    | `Cyan -> 0, 255, 255
    | `Light_cyan -> 224, 255, 255
    | `Dark_turquoise -> 0, 206, 209
    | `Turquoise -> 64, 224, 208
    | `Medium_turquoise -> 72, 209, 204
    | `Pale_turquoise -> 175, 238, 238
    | `Aqua_marine -> 127, 255, 212
    | `Powder_blue -> 176, 224, 230
    | `Cadet_blue -> 95, 158, 160
    | `Steel_blue -> 70, 130, 180
    | `Corn_flowerblue -> 100, 149, 237
    | `Deep_skyblue -> 0, 191, 255
    | `Dodger_blue -> 30, 144, 255
    | `Light_blue -> 173, 216, 230
    | `Skyblue -> 135, 206, 235
    | `Light_sky_blue -> 135, 206, 250
    | `Midnight_blue -> 25, 25, 112
    | `Navy -> 0, 0, 128
    | `Dark_blue -> 0, 0, 139
    | `Medium_blue -> 0, 0, 205
    | `Blue -> 0, 0, 255
    | `Royal_blue -> 65, 105, 225
    | `Blue_violet -> 138, 43, 226
    | `Indigo -> 75, 0, 130
    | `Dark_slate_blue -> 72, 61, 139
    | `Slate_blue -> 106, 90, 205
    | `Medium_slate_blue -> 123, 104, 238
    | `Medium_purple -> 147, 112, 219
    | `Dark_magenta -> 139, 0, 139
    | `Dark_violet -> 148, 0, 211
    | `Dark_orchid -> 153, 50, 204
    | `Medium_orchid -> 186, 85, 211
    | `Purple -> 128, 0, 128
    | `Thistle -> 216, 191, 216
    | `Plum -> 221, 160, 221
    | `Violet -> 238, 130, 238
    | `Magenta | `Fuchsia -> 255, 0, 255
    | `Orchid -> 218, 112, 214
    | `Medium_violet_red -> 199, 21, 133
    | `Pale_violet_red -> 219, 112, 147
    | `Deep_pink -> 255, 20, 147
    | `Hot_pink -> 255, 105, 180
    | `Light_pink -> 255, 182, 193
    | `Pink -> 255, 192, 203
    | `Antique_white -> 250, 235, 215
    | `Beige -> 245, 245, 220
    | `Bisque -> 255, 228, 196
    | `Blanched_almond -> 255, 235, 205
    | `Wheat -> 245, 222, 179
    | `Corn_silk -> 255, 248, 220
    | `Lemon_chiffon -> 255, 250, 205
    | `Light_golden_rod_yellow -> 250, 250, 210
    | `Light_yellow -> 255, 255, 224
    | `Saddle_brown -> 139, 69, 19
    | `Sienna -> 160, 82, 45
    | `Chocolate -> 210, 105, 30
    | `Peru -> 205, 133, 63
    | `Sandy_brown -> 244, 164, 96
    | `Burly_wood -> 222, 184, 135
    | `Tan -> 210, 180, 140
    | `Rosy_brown -> 188, 143, 143
    | `Moccasin -> 255, 228, 181
    | `Navajo_white -> 255, 222, 173
    | `Peach_puff -> 255, 218, 185
    | `Misty_rose -> 255, 228, 225
    | `Lavender_blush -> 255, 240, 245
    | `Linen -> 250, 240, 230
    | `Old_lace -> 253, 245, 230
    | `Papaya_whip -> 255, 239, 213
    | `Seashell -> 255, 245, 238
    | `Mint_cream -> 245, 255, 250
    | `Slate_gray -> 112, 128, 144
    | `Light_slate_gray -> 119, 136, 153
    | `Light_steel_blue -> 176, 196, 222
    | `Lavender -> 230, 230, 250
    | `Floral_white -> 255, 250, 240
    | `Alice_blue -> 240, 248, 255
    | `Ghost_white -> 248, 248, 255
    | `Honeydew -> 240, 255, 240
    | `Ivory -> 255, 255, 240
    | `Azure -> 240, 255, 255
    | `Snow -> 255, 250, 250
    | `Black -> 0, 0, 0
    | `Dim_gray -> 105, 105, 105
    | `Gray -> 128, 128, 128
    | `Dark_gray -> 169, 169, 169
    | `Silver -> 192, 192, 192
    | `Light_gray -> 211, 211, 211
    | `Gainsboro -> 220, 220, 220
    | `White_smoke -> 245, 245, 245
    | `White -> 255, 255, 255
    | `Gray10 -> 26, 26, 26
    | `Gray20 -> 51, 51, 51
    | `Gray30 -> 77, 77, 77
    | `Gray40 -> 102, 102, 102
    | `Gray50 -> 127, 127, 127
    | `Gray60 -> 153, 153, 153
    | `Gray70 -> 179, 179, 179
    | `Gray80 -> 204, 204, 204
    | `Gray90 -> 229, 229, 229
  ;;
end

include M
include Sexpable.To_stringable (M)
