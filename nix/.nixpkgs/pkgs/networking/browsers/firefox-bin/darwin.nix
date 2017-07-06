{ stdenv, lib, fetchurl, undmg }:

let
  appName = "Firefox";
in
stdenv.mkDerivation rec {
  name = "${lib.toLower appName}-darwin-${version}";
  version = "55.0b6";
  dlName = name;

  src = fetchurl {
    url =  "https://archive.mozilla.org/pub/firefox/releases/55.0b6/mac/en-US/Firefox%2055.0b6.dmg";
    sha256 = "01w0ssjl00095rcbrbbv4fz1vhcbcmg1n08p74xfqpngbgda8lxa";
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
