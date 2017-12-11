{ cfg, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices, imagemagick ? null}:

let
  repoName = cfg.repo or "chunkwm";
  repoOwner = cfg.owner or "koekeishiya";
in
stdenv.mkDerivation rec {
  name = "${cfg.name}-${cfg.version}";
  version = "${cfg.version}";

  src = fetchFromGitHub {
    owner = repoOwner;
    repo = repoName;
    rev = "v${cfg.version}";
    sha256 = "${cfg.sha256}";
  };

  buildInputs = [ Carbon Cocoa ApplicationServices ] ++ [ imagemagick ];

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
    homepage = "https://github.com/${repoOwner}/${repoName}";
    downloadPage = "https://github.com/${repoOwner}/${repoName}/releases";
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
