{ config, pkgs, ...}:

{
  services.bloop.install = true;
  environment.systemPackages = with pkgs; [
    # metals
    # haskellEnv
    # ghcide.ghcide-ghc865;
    # haskell-nix.nix-tools
  ];
}
