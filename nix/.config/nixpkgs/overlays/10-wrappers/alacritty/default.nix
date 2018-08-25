{ stdenv, makeWrapper, writeTextFile, alacritty, fonts, colors }:

let
  config = import ./config.nix {
    inherit colors fonts stdenv;
  };
  execPath = if stdenv.isDarwin then "Applications/Alacritty.app/Contents/MacOS" else "bin";
  configFile = writeTextFile {
    name = "alacritty.yml";
    text = config;
  };
in
stdenv.mkDerivation {
  name = "alacrittyConf";
  buildInputs = [ makeWrapper ];
  propagatedBuildInputs = [ alacritty ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${alacritty}/${execPath}/alacritty $out/bin/alacritty --add-flags "--config-file ${configFile}"
  '';
}
