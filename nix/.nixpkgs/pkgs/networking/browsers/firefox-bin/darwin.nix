{ stdenv, lib, fetchurl, undmg }:

let
  appName = "Firefox";
in
stdenv.mkDerivation rec {
  name = "${lib.toLower appName}-darwin-${version}";
  version = "57.0b7";
  dlName = name;

  src = fetchurl {
    url =  "https://archive.mozilla.org/pub/firefox/releases/57.0b7/mac/en-US/Firefox%2057.0b7.dmg";
    sha256 = "0g2n46j3xai2xxay4h3mkbjjy5rhqhh5l6jg59h61bvbzmnvxgah";
    name = "${ dlName }.dmg";
  };

  buildInputs = [ undmg ];
  installPhase = ''
    mkdir -p "$out/Applications/${appName}.app"
    cp -R . "$out/Applications/${appName}.app"
  '';

  meta = with stdenv.lib; {
    description = "Mozilla Firefox, free web browser (binary package)";
    homepage = http://www.mozilla.org/firefox/;
    license = {
      free = false;
      url = http://www.mozilla.org/en-US/foundation/trademarks/policy/;
    };
    platforms = platforms.darwin;
  };
}
