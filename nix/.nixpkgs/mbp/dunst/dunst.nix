{ pkgs, browser }:
let
  colors = import ../setup/colors.nix;
  dunst-config = import ./dunst-conf.nix {
    inherit colors browser;
  };
  dunst-config-file = pkgs.writeTextFile {
    name = "dunst-xresources";
    text = dunst-config;
  };
in
pkgs.stdenv.mkDerivation {
  name = "dunstWrapper";
  buildInputs = [ pkgs.makeWrapper ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper "${pkgs.dunst}/bin/dunst" $out/bin/dunst --add-flags "-config ${dunst-config-file}"
  '';
}
