{ pkgs, fonts, colors }:
let
  config = import ./config.nix {
    inherit colors fonts;
  };
  configFile = pkgs.writeTextFile {
    name = "alacritty.yml";
    text = config;
  };
in
pkgs.stdenv.mkDerivation {
  name = "alacrittyWrapper";
  buildInputs = with pkgs; [ alacritty makeWrapper ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.alacritty}/bin/alacritty $out/bin/alacritty --add-flags "--config-file ${configFile}"
  '';
}
