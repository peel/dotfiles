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
    darwin-rebuild = "darwin-rebuild --option extra-builtins-file ${<dotfiles/setup/common/secrets/extra-builtins.nix>}";
  };
  
  nix.extraOptions = ''
    builders = @/etc/nix/machines
  '';

  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];
  networking.dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];
  
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  programs.nix-index.enable = true;

}
