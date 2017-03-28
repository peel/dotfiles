{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "hoverfly-${version}";
  version = "v0.11.0";
  rev = "${version}";

  goPackagePath = "github.com/SpectoLabs/hoverfly";

  src = fetchFromGitHub {
    inherit rev;
    owner = "SpectoLabs";
    repo = "hoverfly";
    sha256 = "08ah19an4gdlna6vcfi8qm1d5jxdjnvssgxnl113s6bjwhgyrisa";
  };
  meta = with stdenv.lib; {
    description = "Lightweight service virtualization/API simulation tool for developers and testers";
    homepage = "http://hoverfly.io";
    license = licenses.asl20;
  };
}
