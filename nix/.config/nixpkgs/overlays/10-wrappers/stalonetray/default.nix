{ pkgs }:
let
  colors = import ../colors.nix;
  stalonetray-config = import ./stalonetray-conf.nix {
    inherit colors;
  };
  stalonetray-config-file = pkgs.writeTextFile {
    name = "stalonetray-xresources";
    text = stalonetray-config;
  };
in
pkgs.stdenv.mkDerivation {
  name = "stalonetrayWrapper";
  buildInputs = [ pkgs.makeWrapper ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper "${pkgs.stalonetray}/bin/stalonetray" $out/bin/stalonetray --add-flags -"c ${stalonetray-config-file}"
  '';
}
