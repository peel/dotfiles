{ stdenv, fetchFromGitHub, Carbon, Cocoa }:

stdenv.mkDerivation rec {
  name = "chunkwm-core-${version}";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "chunkwm";
    rev = "core-${version}";
    sha256 = "1kh87kby33qpkss0j4xf2dbqam087bsmng78nbasjb2ca0m40mmv";
  };

  buildInputs = [ Carbon Cocoa ];

  #HACKY way to get macOS' clang++
  prePatch = ''
    substituteInPlace makefile \
      --replace clang++ /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++
  '';

  buildPhase = ''
    PATH=$PATH:/System/Library/Frameworks make install
    clang $src/src/chunkc/chunkc.c -O2 -o ./bin/chunkc
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin/

    mkdir -p $out/Library/LaunchDaemons
    cp ${./org.nixos.chunkwm.plist} $out/Library/LaunchDaemons/org.nixos.chunkwm.plist
    substituteInPlace $out/Library/LaunchDaemons/org.nixos.chunkwm.plist --subst-var out
  '';

  meta = with stdenv.lib; {
    description = "A tiling window manager for macOS";
    homepage = https://github.com/koekeishiya/chunkwm;
    downloadPage = https://github.com/koekeishiya/chunkwm/releases;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
