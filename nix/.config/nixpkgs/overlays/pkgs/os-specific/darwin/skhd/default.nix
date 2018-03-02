{stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices}:

stdenv.mkDerivation rec {
  version = "0.0.6";
  name = "skhd-${version}";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "skhd";
    rev = "v${version}";
    sha256 = "0vp2cag693x0jdpdqch5nq2qdgj81jx00v3mv69y6r40iy2hiw3w";
  };

  buildInputs = [ ApplicationServices Carbon Cocoa ];

  prePatch = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -F/System/Library/Frameworks"
    substituteInPlace makefile \
      --replace clang /usr/bin/clang
  '';

  buildPhase = ''
    PATH=$PATH:/System/Library/Frameworks make install
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin/

    mkdir -p $out/Library/LaunchDaemons
    cp ${./org.nixos.skhd.plist} $out/Library/LaunchDaemons/org.nixos.skhd.plist
    substituteInPlace $out/Library/LaunchDaemons/org.nixos.skhd.plist --subst-var out
  '';

  meta = with stdenv.lib; {
    description = "Simple hotkey daemon for macOS";
    homepage = https://github.com/koekeishiya/skhd;
    downloadPage = https://github.com/koekeishiya/skhd/releases;
    platforms = platforms.darwin;
    license = licenses.mit;
  };
}
