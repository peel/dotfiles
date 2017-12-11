{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "gopass-${version}";
  version = "v1.6.2";
  rev = "${version}";

  goPackagePath = "github.com/justwatchcom/gopass";

  src = fetchFromGitHub {
    inherit rev;
    owner = "justwatchcom";
    repo = "gopass";
    sha256 = "14miz0v2ms59agbfnq4mq9kbfhvhnw8yf15vbya7gina2cgrj22y";
  };
  meta = with stdenv.lib; {
    description = "The slightly more awesome standard unix password manager for teams";
    homepage = "https://www.justwatch.com/gopass";
    license = licenses.mit;
  };
}
