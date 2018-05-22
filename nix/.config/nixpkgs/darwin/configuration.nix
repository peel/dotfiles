{config, lib, pkgs, ...}:

with (import ../libs { inherit lib; });

let
  colors = (import ../setup/colors.nix { theme = "dark"; });
  fonts = import ../setup/fonts.nix;
  username = "peel";
  hostName = "fff666";
in rec {
  system.stateVersion = 3;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = [ (import (mkOverlay username username)) ];
  nix.package = pkgs.nix;
  nix.useSandbox = true;
  nix.maxJobs = 4;
  nix.nixPath =
    [
      "darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix"
      "darwin=$HOME/.nix-defexpr/channels/darwin"
      "nixpkgs=/nix/var/nix/profiles/per-user/peel/channels/nixpkgs"
      "$HOME/.nix-defexpr/channels"
    ];
  networking.hostName = hostName;

  require = [
    ../setup/common.nix
    ../setup/darwin.nix
    ../setup/packages.nix
    ../setup/emacs.nix
  ] ++ import (mkOverlay username "darwin-modules/module-list");

  nixpkgs.config.packageOverrides = pkgs : rec {
    alacrittyDrv = pkgs.callPackage (builtins.toPath "/Users/${username}/.config/nixpkgs/overlays/pkgs/applications/misc/alacritty") {};
    alacrittyWrapper = import ../setup/alacritty {
      inherit colors fonts;
      inherit (pkgs) stdenv makeWrapper writeTextFile;
      alacritty = alacrittyDrv;
    };
    emacs = import ../setup/emacs { inherit pkgs; }; 
  };
}
