{ config, pkgs, ... }:

{
  fonts = {
    fontconfig.enable = true;
    fontconfig.dpi = 180;
  };
}
