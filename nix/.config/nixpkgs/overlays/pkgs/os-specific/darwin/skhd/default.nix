{ stdenv, fetchFromGitHub, Carbon }:

stdenv.mkDerivation rec {
  name = "skhd-${version}";
  version = "0.0.10";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = "skhd";
    rev = "v${version}";
    sha256 = "0a0r8z9bvb1pzqag7nqa84xm99n0xvg27cw11qcv65snr06bqc9w";
  };

  hardeningDisable = [ "all" ];

  buildInputs = [ Carbon ];

  makeFlags = [ "BUILD_PATH=$(out)/bin" ];

  meta = with stdenv.lib; {
    description = "Simple hotkey daemon for macOS";
    homepage = https://github.com/koekeishiya/skhd;
    platforms = platforms.darwin;
    maintainers = with maintainers; [ lnl7 ];
    license = licenses.mit;
  };
}
