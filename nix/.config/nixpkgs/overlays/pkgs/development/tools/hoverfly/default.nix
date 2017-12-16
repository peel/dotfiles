{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "hoverfly-${version}";
  version = "v0.14.2";
  rev = "${version}";

  goPackagePath = "github.com/SpectoLabs/hoverfly";

  src = fetchFromGitHub {
    inherit rev;
    owner = "SpectoLabs";
    repo = "hoverfly";
    sha256 = "0aq0izfrdszb65cw8afkdd36pa63shd54yhhycv4yz86csh8na9k";
  };
  meta = with stdenv.lib; {
    description = "Lightweight service virtualization/API simulation tool for developers and testers";
    homepage = "http://hoverfly.io";
    license = licenses.asl20;
  };
}
