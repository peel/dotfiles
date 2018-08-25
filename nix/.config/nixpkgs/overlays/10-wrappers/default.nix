self: super:

{
  alacritty = import ./alacritty {
    inherit (self) colors fonts;
    inherit (super) stdenv makeWrapper writeTextFile alacritty;
  };
  emacs = import ./emacs { pkgs = super; };
  rofi = import ./rofi { inherit (super) colors fonts; pkgs = super; terminal = "alacritty"; };
  urxvt = import ./urxvt { inherit (super) colors fonts; pkgs = super; };
  dunst = import ./dunst { inherit (super) colors fonts; pkgs = super; browser = "firefox"; };
  stalonetray = import ./stalonetray { pkgs = super; };
}
