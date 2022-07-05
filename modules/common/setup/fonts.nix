{ config, pkgs, lib, ... }:

{
  fonts = {
    fontDir.enable = true;
    fonts = [ pkgs.pragmatapro ];
  } // lib.optionalAttrs pkgs.stdenvNoCC.isLinux {
    fontconfig.enable = true;
    enableDefaultFonts = false;
    fontconfig.defaultFonts.monospace = ["PragmataPro Liga"];
    fontconfig.defaultFonts.sansSerif = ["PragmataPro Liga"];
    fontconfig.defaultFonts.serif = ["PragmataPro Liga"];
    fontconfig.hinting.enable = false;
    fontconfig.antialias = true;
  };
}
