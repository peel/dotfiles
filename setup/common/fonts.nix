{ config, pkgs, lib, ... }:

let
  pragmatapro = pkgs.callPackage ./pragmatapro {};
in {
  fonts = {
    enableFontDir = true;
    fonts = [pragmatapro];
  } // lib.optionalAttrs pkgs.stdenvNoCC.isLinux {
    fontconfig.enable = true;
    enableDefaultFonts = false;
    fontconfig.defaultFonts.monospace = ["PragmataPro"];
    fontconfig.defaultFonts.sansSerif = ["PragmataPro"];
    fontconfig.defaultFonts.serif = ["PragmataPro"];
  };
}
