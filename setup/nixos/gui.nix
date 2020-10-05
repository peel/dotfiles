{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.peel.gui;
in {
  options.peel.gui.enable = mkEnableOption "gui";
  
  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.acpi
      pkgs.arandr
      pkgs.autorandr
      pkgs.blueman
      pkgs.bluez
      pkgs.dunst
      pkgs.feh
      pkgs.firefox
      pkgs.peelxmonad
      pkgs.xmobar
      pkgs.haskellPackages.xmonad
      pkgs.haskellPackages.xmonad-contrib
      pkgs.haskellPackages.xmonad-extras
      pkgs.haskellPackages.yeganesh
      pkgs.iw
      pkgs.keybase
      pkgs.keybase-gui
      pkgs.libnotify
      pkgs.lightum
      pkgs.powertop
      pkgs.rofi
      pkgs.rofi-emoji
      pkgs.rofi-pass
      pkgs.rofi-wifi-menu
      pkgs.scrot
      pkgs.stalonetray
      pkgs.unclutter-xfixes
      pkgs.wirelesstools
      pkgs.xclip
      pkgs.xsel
      pkgs.xfce.thunar
      pkgs.xfce.thunar-archive-plugin
      pkgs.xfce.thunar-dropbox-plugin
      pkgs.xfce.thunar_volman
      pkgs.xfce.xfce4_power_manager
      pkgs.xorg.libXrandr
      pkgs.xorg.xbacklight
      pkgs.xorg.xcursorthemes
      pkgs.xorg.xf86inputkeyboard
      pkgs.zeal
    ];
  };
}
