{ pkgs }:
let
  colors = import ../colors.nix;
  config = import ./config.nix {
    inherit colors;
  };
  kitty-config-file = pkgs.writeTextFile {
    name = "kitty.conf";
    text = config;
  };
in
pkgs.stdenv.mkDerivation {
  name = "kittyWrapper";
  buildInputs = with pkgs; [ kitty makeWrapper ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.kitty}/bin/kitty $out/bin/kitty --add-flags "--config ${kitty-config-file}"
  '';
}
