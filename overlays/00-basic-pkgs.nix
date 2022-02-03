self: pkgs:

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
in {
  haskell-nix = import sources."haskell-nix" {};
  yabaiM1 = pkgs.stdenvNoCC.mkDerivation {
    name = "yabai";
    version = "4.0.0-pre-91a42e7";
    src = pkgs.fetchurl {
      url = "https://github.com/koekeishiya/yabai/files/7915231/yabai-v4.0.0.tar.gz";
      sha256 = "RyuUEtv5HR/ZyKhpH81UzIx9CO4q/IMcPFUdtoVFAyI=";
    };

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p $out
      cp -ar ./* $out
      chmod +x $out/bin/yabai
    '';
  };
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
