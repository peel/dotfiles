{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "hoverfly-${version}";
  version = "v0.15.0";
  rev = "${version}";

  goPackagePath = "github.com/SpectoLabs/hoverfly";

  src = fetchFromGitHub {
    inherit rev;
    owner = "SpectoLabs";
    repo = "hoverfly";
    sha256 = "1vyglqqpm7845cavs415k975vz9pbczrxxhr2nvd1a4bc62qakqj";
  };
  meta = with stdenv.lib; {
    description = "Lightweight service virtualization/API simulation tool for developers and testers";
    homepage = "http://hoverfly.io";
    license = licenses.asl20;
  };
}
