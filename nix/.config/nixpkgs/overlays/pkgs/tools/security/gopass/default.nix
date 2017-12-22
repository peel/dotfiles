{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "gopass-${version}";
  version = "v1.6.6";
  rev = "${version}";

  goPackagePath = "github.com/justwatchcom/gopass";

  src = fetchFromGitHub {
    inherit rev;
    owner = "justwatchcom";
    repo = "gopass";
    sha256 = "0n3isjrjpn2cnlwfdkjdcz5j8n16dhyaw4zyjpmis51nl0bqd3jw";
  };
  meta = with stdenv.lib; {
    description = "The slightly more awesome standard unix password manager for teams";
    homepage = "https://www.justwatch.com/gopass";
    license = licenses.mit;
  };
}
