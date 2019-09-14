{ config, pkgs, lib, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = [] ++ lib.optionals (pkgs ? pragmatapro) [
      pkgs.pragmatapro
    ];
  } // lib.optionalAttrs pkgs.stdenvNoCC.isLinux {
    fontconfig.enable = true;
    fontconfig.dpi = 180;
  };
}
