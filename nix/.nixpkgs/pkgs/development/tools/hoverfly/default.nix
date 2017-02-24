{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "hoverfly-${version}";
  version = "v0.10.2";
  rev = "${version}";

  goPackagePath = "github.com/SpectoLabs/hoverfly";

  src = fetchFromGitHub {
    inherit rev;
    owner = "SpectoLabs";
    repo = "hoverfly";
    sha256 = "1ffihn4yypz9cf22rl2mjxv28hy3lx4bk93rh14wswx1g5v8dh6p";
  };
  meta = with stdenv.lib; {
    description = "Lightweight service virtualization/API simulation tool for developers and testers";
    homepage = "http://hoverfly.io";
    license = licenses.asl20;
  };
}
