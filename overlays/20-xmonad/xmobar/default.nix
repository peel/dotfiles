{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "xmobarWrapper";
  buildInputs = [ pkgs.makeWrapper ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper "${pkgs.xmobar}/bin/xmobar" $out/bin/xmobar --add-flags "-x0 ${./xmobarrc}"
  '';
}
