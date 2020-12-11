{ config, lib, pkgs, ... }:

{
  services.home-assistant = {
    enable = true;
    applyDefaultConfig = true;
    configWritable = true;
    openFirewall = true;
    autoExtraComponents = true;
  };
}
