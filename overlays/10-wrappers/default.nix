self: super:

# let
#   fonts = {
#     family = {
#       default = "PragmataPro";
#       mono = "PragmataPro Mono";
#       emoji = "EmojiOne";
#     };
#     size = {
#       small = "14";
#       default = "16";
#       large = "22";
#     };
#   };
#   colors = rec {
#     dark = true;
#     foreground =   "#98d1ce";
#     background =   "#0a0f14";
#     cursorColor =  "#98d1ce";

#     #black
#     black   = color0;
#     color0  = "#0a0f14";
#     color8  = "#10151b";
#     #red
#     red     = color1;
#     color1  = "#c33027";
#     color9  = "#d26939";
#     #green
#     green   = color2;
#     color2  = "#26a98b";
#     color10 = "#081f2d";
#     #yellow
#     yellow  = color3;
#     color3  = "#edb54b";
#     color11 = "#245361";
#     #blue
#     blue    = color4;
#     color4  = "#195465";
#     color12 = "#093748";
#     #magenta
#     magenta = color5;
#     color5  = "#4e5165";
#     color13 = "#888ba5";
#     #cyan
#     cyan    = color6;
#     color6  = "#33859d";
#     color14 = "#599caa";
#     #white
#     white   = color7;
#     color7  = "#98d1ce";
#     color15 = "#d3ebe9";
#   };
# in
{
#   alacritty = import ./alacritty {
#     colors = colors;
#     fonts = fonts;
#     inherit (super) stdenv makeWrapper writeTextFile alacritty;
#   };
#   rofi = import ./rofi {     colors = colors;
#     fonts = fonts;
#  pkgs = super; terminal = "alacritty"; };
#   urxvt = import ./urxvt {     colors = colors;
#     fonts = fonts;
#  pkgs = super; };
#   dunst = import ./dunst {     colors = colors;
#     fonts = fonts;
#  pkgs = super; browser = "firefox"; };
#   stalonetray = import ./stalonetray { pkgs = super; };
  sentinelone = import ./sentinelone.nix { inherit (super)  stdenv lib autoPatchelfHook dpkg zlib libelf; };
}
