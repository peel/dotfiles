let
    unstable = import <unstable> {};
    nixpkgs = import <nixpkgs> {};
in
with nixpkgs;
{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nix.package = nixpkgs.nixUnstable;
  # nix.nixPath =
  #   [ # Use local nixpkgs checkout instead of channels.
  #     "darwin=$HOME/.nix-defexpr/darwin"
  #     "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
  #     "nixpkgs=/nix/var/nix/profiles/per-user/peel/channels/nixpkgs"
  #     "/nix/var/nix/profiles/per-user/peel/channels"
  #   ];

  require = [
    ./setup/common.nix
    ./setup/darwin.nix
    ./setup/packages.nix
    ./setup/emacs.nix
  ];
}
