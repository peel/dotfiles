{ lib }:

rec {
  mkOverlay = username: overlay: builtins.toPath "/Users/${username}/.config/nixpkgs/overlays/${overlay}.nix";
}
