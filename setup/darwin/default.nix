{ config, pkgs, ... }:

{
  imports = [
    ./defaults.nix
    ./keyboard.nix
    ./wm.nix
    ./brew.nix
  ];

  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];
  networking.dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  programs.nix-index.enable = true;
}
