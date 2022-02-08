{ config, pkgs, lib, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = [ pkgs.pragmatapro ];
  } // lib.optionalAttrs pkgs.stdenvNoCC.isLinux {
    fontconfig.enable = true;
    enableDefaultFonts = false;
    fontconfig.defaultFonts.monospace = ["PragmataPro"];
    fontconfig.defaultFonts.sansSerif = ["PragmataPro"];
    fontconfig.defaultFonts.serif = ["PragmataPro"];
  };
}
