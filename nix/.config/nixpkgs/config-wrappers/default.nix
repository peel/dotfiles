{colors, fonts}: self: super:

{
  alacritty = import ./alacritty {
    inherit colors fonts;
    inherit (super) stdenv makeWrapper writeTextFile alacritty;
  };
  emacs = import ./emacs { pkgs = super; };
  rofi = import ./rofi { inherit colors fonts; pkgs = super; terminal = "alacritty"; };
  urxvt = import ./urxvt { inherit colors fonts; pkgs = super; };
  dunst = import ./dunst { inherit colors fonts; pkgs = super; browser = "firefox"; };
  stalonetray = import ./stalonetray { pkgs = super; };
}
