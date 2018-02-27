{ stdenv, fetchurl, coreutils, curl, emojione, rofi, xsel, xdotool, xclip, libxml2, libnotify, makeWrapper }:

stdenv.mkDerivation rec {
  version = "1.0.0";
  baseName = "rofi-emoji";
  name = "${baseName}-${version}";

  src = fetchurl {
    sha256 = "0iglznr65s766gh2lxpf5yd7ddb6w83npqq2c0h11fp2x9p1vd73";
    url = "https://gist.githubusercontent.com/Tadly/0741821d3694deaec1ee454a95c591fa/raw/8a4e3b9712f06737e50c73e30345b20b538fd1ef/rofi-emoji.sh";
  };

  buildInputs = [ makeWrapper xdotool ];
  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/${baseName}
    chmod a+x $out/bin/${baseName}
  '';

  wrapperPath = with stdenv.lib; makeBinPath [
    coreutils
    curl
    emojione
    libnotify
    libxml2
    rofi
    xsel
    xclip
    xdotool
  ];

  fixupPhase = ''
    patchShebangs $out/bin
    wrapProgram $out/bin/${baseName} --prefix PATH : "${wrapperPath}"
  '';

  meta = with stdenv.lib; {
    description = "A rofi emoji picker";
    platforms = platforms.linux;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
