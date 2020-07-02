{ config, pkgs, ... }:

{

  imports = [
    ../common
    ./gui.nix
  ];
  
  environment.systemPackages = with pkgs; [
    docker            
    docker_compose
    pinentry
  ];

  environment.shellAliases = {
    nixos-rebuild = "nixos-rebuild --option extra-builtins-file ${<dotfiles/setup/common/secrets/extra-builtins.nix>}";
  };
}
