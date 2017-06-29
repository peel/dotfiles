{ stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

stdenv.mkDerivation rec {
  name = "chunkwm-border-${version}";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "chunkwm";
    rev = "border-${version}";
    sha256 = "13x39gimqfhfxrdq0xzr59kxmisbzl19gjpvk0x1hhndnfhnisgz";
  };

  buildInputs = [ Carbon Cocoa ApplicationServices ];

  buildPhase = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -F/System/Library/Frameworks"
    substituteInPlace $src/src/plugins/border/makefile --replace "clang++" "/usr/bin/clang++"
    substituteInPlace $src/src/plugins/border/makefile --replace "BUILD_PATH		= ./../../../plugins"  "BUILD_PATH		= /tmp/chunkwm-plugins"
    substituteInPlace $src/src/plugins/border/makefile --replace "./../../" ${src}/src/
    cd $src/src/plugins/border && make install
  '';

  installPhase = ''
    mkdir -p $out/lib/chunkwm/plugins
    cp /tmp/chunkwm-plugins/border.so $out/lib/chunkwm/plugins/
    rm -rf /tmp/chunkwm-plugins
  '';

  meta = with stdenv.lib; {
    description = "A ChunkWM plugin for drawing borders around windows";
    homepage = https://github.com/koekeishiya/chunkwm;
    downloadPage = https://github.com/koekeishiya/chunkwm/releases;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
