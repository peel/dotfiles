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

  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };
}
