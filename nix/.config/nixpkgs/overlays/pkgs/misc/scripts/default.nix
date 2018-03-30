{ pkgs, stdenv }:

with stdenv.lib;

stdenv.mkDerivation rec {
  version = "1.0.0";
  baseName = "peel-scripts";
  name = "${baseName}-${version}";

  buildInputs = with pkgs; [ coreutils ]
    ++ [ ripgrep gnome3.zenity ]  # qmk
    ++ [ curl fzf fasd ]; # z
  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp -r ${./bin}/* $out/bin/
    for f in $out/bin/*; do
      chmod a+x $f
    done
    patchShebangs $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Some useful scripts I often use";
    platforms = platforms.unix;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
