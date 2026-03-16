self: pkgs:

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
in {
  nix-plugins-latest =
    (pkgs.nix-plugins.override { nix = self.nixFlakes; }).overrideAttrs
      (oldAttrs: {
        src = pkgs.fetchFromGitHub {
          owner = "shlevy";
          repo = "nix-plugins";
          rev = "d0df32b31f3054180741adf5865fd56d6731c572";
          sha256 = "Zbc0iq5ZAr73B+NJvpBHm9GIJhb9qrq0vFmV/ucNT5I=";
        };
        buildInputs = oldAttrs.buildInputs ++ [ pkgs.nlohmann_json ];
      });
}
