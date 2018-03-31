{ pkgs, stdenv }:

with stdenv.lib;

stdenv.mkDerivation rec {
  version = "1.0.0";
  baseName = "peel-scripts";
  name = "${baseName}-${version}";

  buildInputs = [ pkgs.makeWrapper ];
  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp -r ${./bin}/* $out/bin/
    for f in $out/bin/*; do
      chmod a+x $f
      wrapProgram $f --prefix PATH : "${wrapperPath}"
    done
  '';

  wrapperPath = with stdenv.lib; makeBinPath (with pkgs;
       [ coreutils ]
    ++ [ ripgrep zenity ]  # qmk
    ++ [ curl fzf fasd ]); # z

  meta = with stdenv.lib; {
    description = "Some useful scripts I often use";
    platforms = platforms.unix;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
