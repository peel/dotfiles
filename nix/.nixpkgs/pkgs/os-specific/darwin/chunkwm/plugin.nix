{ cfg, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

stdenv.mkDerivation rec {
  name = "${cfg.name}-${cfg.version}";
  version = "${cfg.version}";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "chunkwm";
    rev = "v${cfg.version}";
    sha256 = "${cfg.sha256}";
  };

  buildInputs = [ Carbon Cocoa ApplicationServices ];

  buildPhase = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -F/System/Library/Frameworks"
    substituteInPlace $src/src/plugins/${cfg.name}/makefile --replace "clang++" "/usr/bin/clang++"
    substituteInPlace $src/src/plugins/${cfg.name}/makefile --replace "BUILD_PATH		= ./../../../plugins"  "BUILD_PATH		= /tmp/chunkwm-plugins"
    substituteInPlace $src/src/plugins/${cfg.name}/makefile --replace "./../../" ${src}/src/
    cd $src/src/plugins/${cfg.name} && make install
  '';

  installPhase = ''
    mkdir -p $out/lib/chunkwm/plugins
    cp /tmp/chunkwm-plugins/${cfg.name}.so $out/lib/chunkwm/plugins/
    rm -rf /tmp/chunkwm-plugins
  '';

  meta = with stdenv.lib; {
    description = "A ChunkWM plugin for ${cfg.name}";
    homepage = https://github.com/koekeishiya/chunkwm;
    downloadPage = https://github.com/koekeishiya/chunkwm/releases;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
