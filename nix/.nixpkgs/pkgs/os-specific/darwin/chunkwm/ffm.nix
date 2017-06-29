{ stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

stdenv.mkDerivation rec {
  name = "chunkwm-ffm-${version}";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "chunkwm";
    rev = "ffm-${version}";
    sha256 = "024gxjxy5rlwxwiz3jk7zd053cinjc8hmlnxvkkan5j9gd51ph9n";
  };

  buildInputs = [ Carbon Cocoa ApplicationServices ];

  buildPhase = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -F/System/Library/Frameworks"
    substituteInPlace $src/src/plugins/ffm/makefile --replace "clang++" "/usr/bin/clang++"
    substituteInPlace $src/src/plugins/ffm/makefile --replace "BUILD_PATH		= ./../../../plugins"  "BUILD_PATH		= /tmp/chunkwm-plugins"
    substituteInPlace $src/src/plugins/ffm/makefile --replace "./../../" ${src}/src/
    cd $src/src/plugins/ffm && make install
  '';

  installPhase = ''
    mkdir -p $out/lib/chunkwm/plugins
    cp /tmp/chunkwm-plugins/ffm.so $out/lib/chunkwm/plugins/
    rm -rf /tmp/chunkwm-plugins
  '';

  meta = with stdenv.lib; {
    description = "A ChunkWM plugin for ffm";
    homepage = https://github.com/koekeishiya/chunkwm;
    downloadPage = https://github.com/koekeishiya/chunkwm/releases;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ peel ];
    license = licenses.mit;
  };
}
