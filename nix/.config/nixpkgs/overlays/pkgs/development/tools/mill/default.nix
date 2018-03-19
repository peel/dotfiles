{ stdenv, fetchurl, makeWrapper, jre }:

stdenv.mkDerivation rec {
  name = "mill-${version}";
  version = "0.1.4";

  src = fetchurl {
    url = "https://github.com/lihaoyi/mill/releases/download/${version}/${version}";
    sha256 = "05n5wh4iyp40mkp67654sj6zssf7lljpsv40g0fcnlwrl5m9s0n8";
  };

  nativeBuildInputs = [ makeWrapper ];

  phases = "installPhase";

  installPhase = ''
    mkdir -p $out/bin
    cp ${src} $out/bin/mill
    chmod +x $out/bin/mill
    wrapProgram $out/bin/mill --prefix PATH ":" ${jre}/bin ;
  '';

  meta = with stdenv.lib; {
    homepage = http://lihaoyi.com/mill/;
    description = "Your shiny new Scala build tool! ";
    license = licenses.mit;
  };
}
