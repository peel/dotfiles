{ config, pkgs, ... }:

{
  imports = [
    ../common
    ../common/weechat.nix
    ./defaults.nix
    ./wm.nix
  ];

  environment.systemPackages = with pkgs; [
    skhd
    Alfred
    Dash
    Docker
    pinentry_mac
    yabai
  ];

  environment.shellAliases = {
    pbc = "pbcopy";
    pbp = "pbpaste";
    o = "open";
    nr = "darwin-rebuild";
  };
  
  nix.extraOptions = ''
      builders = @/etc/nix/machines
  '';

  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  programs.nix-index.enable = true;
 
}
