{ pkgs, config, lib, ...}:

{
  system.stateVersion = 4;
  networking.hostName = "peel-sp-mbp";
  # FIXME AppleSilicon
  nixpkgs.config.allowBroken = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
