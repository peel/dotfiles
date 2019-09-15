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
  
}
