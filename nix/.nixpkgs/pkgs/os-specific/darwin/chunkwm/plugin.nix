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
    substituteInPlace src/plugins/${cfg.name}/makefile --replace "clang++" "/usr/bin/clang++"
  '';

  installPhase = ''
    cd src/plugins/${cfg.name} && make all
    mkdir -p $out/lib/chunkwm/plugins
    cp ../../../plugins/${cfg.name}.so $out/lib/chunkwm/plugins/
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
