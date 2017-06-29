{ stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

stdenv.mkDerivation rec {
  name = "chunkwm-tiling-${version}";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "chunkwm";
    rev = "tiling-${version}";
    sha256 = "1sim1zpjcs8ki7mq0md42h67d444biydym3w79gvqy5r5g00qx2a";
  };

  buildInputs = [ Carbon Cocoa ApplicationServices ];

  buildPhase = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -F/System/Library/Frameworks"
    substituteInPlace $src/src/plugins/tiling/makefile --replace "clang++" "/usr/bin/clang++"
    substituteInPlace $src/src/plugins/tiling/makefile --replace "BUILD_PATH		= ./../../../plugins"  "BUILD_PATH		= /tmp/chunkwm-plugins"
    substituteInPlace $src/src/plugins/tiling/makefile --replace "./../../" ${src}/src/
    cd $src/src/plugins/tiling && make install
  '';

  installPhase = ''
    mkdir -p $out/lib/chunkwm/plugins
    cp /tmp/chunkwm-plugins/tiling.so $out/lib/chunkwm/plugins/
    cp $src/src/plugins/tiling/examples/chwmtilingrc $out/.chunkwmtilingrc
    rm -rf /tmp/chunkwm-plugins
  '';

  meta = with stdenv.lib; {
    description = "A ChunkWM plugin for drawing tilings around windows";
    homepage = https://github.com/koekeishiya/chunkwm;
    downloadPage = https://github.com/koekeishiya/chunkwm/releases;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
