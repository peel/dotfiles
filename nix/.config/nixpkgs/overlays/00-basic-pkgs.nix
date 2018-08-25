self: super: 

{
  hex2nix = super.hex2nix.overrideAttrs(oldAttrs: rec {
    name = "hex2nix-${version}";
    version = "0.6.0-786de2e";
    src = super.fetchFromGitHub {
      owner = "erlang-nix";
      repo = "hex2nix";
      rev = "786de2e4a0633c4a24dc5c3904e0f9ffa9557a7d";
      sha256 = "04knhxjk4jyc2j7a32i6wkz2msfiw14q4l9gc1kq90nfpv0dxfiv";
    };
  });
}
