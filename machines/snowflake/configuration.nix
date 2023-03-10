{ pkgs, config, lib, ...}:

{
  system.stateVersion = 4;
  # FIXME AppleSilicon
  #nixpkgs.config.allowBroken = true;
  #nixpkgs.config.allowUnsupportedSystem = true;
}
