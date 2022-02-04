{ config, pkgs, ... }:

let
  home = builtins.getEnv "HOME";
in {

  imports = [
    ../common
  ];

  environment.systemPackages = [
    pkgs.docker
    pkgs.docker_compose
  ];

  environment.shellAliases = {
    nixos-rebuild = "nixos-rebuild --option extra-builtins-file ${home}/setup/common/secrets/extra-builtins.nix";
  };

  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };
}
