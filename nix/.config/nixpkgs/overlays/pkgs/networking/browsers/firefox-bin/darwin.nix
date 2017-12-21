{ stdenv, lib, fetchurl, undmg }:

assert stdenv.isDarwin;

let
  appName = "Firefox";
in
stdenv.mkDerivation rec {
  name = "${lib.toLower appName}-darwin-${version}";
  version = "57.0";
  dlName = name;

  src = fetchurl {
    url =  "https://archive.mozilla.org/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
    sha256 = "1alb8qjw8k8j7dsqdmdjbsri02lzbih1xpvkpgcic4k5qawzr0xf";
    name = "${ dlName }.dmg";
  };

  buildInputs = [ undmg ];
  installPhase = ''
    mkdir -p "$out/Applications/${appName}.app"
    cp -R . "$out/Applications/${appName}.app"
  '';

  postInstall = ''
    ln -f $out/Applications/${appName}.app ~/Applications/${appName}.app
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
