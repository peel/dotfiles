{ config, pkgs, ... }:

let
  home = builtins.getEnv "HOME";
in {
  imports = [
    ./config.nix
    ../common
    ./defaults.nix
    ./keyboard.nix
    ./wm.nix
    ./brew.nix
  ];

  environment.shellAliases = {
    o = "open";
  };

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    builders = @/etc/nix/machines
    experimental-features = nix-command flakes
  '';

  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];
  networking.dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  programs.nix-index.enable = true;
}
