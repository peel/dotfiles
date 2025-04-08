{ config, pkgs, ... }:

{
  imports = [
    ./defaults.nix
    ./keyboard.nix
    ./wm.nix
    ./brew.nix
  ];

  users.users.peel.home = "/Users/peel";
  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];
  # networking.dns = ["45.90.28.76" "45.90.30.76" "2a07:a8c0::b6:c347" "2a07:a8c1::b6:c347"];
  services.nextdns = {
    enable = false;
    arguments = ["-profile" "b6c347"];
  };

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  programs.nix-index.enable = true;
  nix.linux-builder = {
    enable = true;
    systems = [ "aarch64-linux" ];
  };
  nixpkgs.config.allowBroken = false;
}
