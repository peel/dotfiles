{ config, pkgs, headless, ... }:

{
  environment.systemPackages = with pkgs; ([
    coreutils
    direnv
    emacs
    git
    gitAndTools.git-crypt
    gnupg
    metals
    ripgrep
    scripts
    haskellEnv
  ]
  ++ lib.optionals stdenv.isLinux [
    docker            
    docker_compose
    htop
    pinentry
  ]
  ++ lib.optionals (stdenv.isLinux && headless == false) [
    acpi
    arandr
    autorandr
    blueman
    bluez
    dunst
    feh
    firefox
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.yeganesh
    iw
    keybase
    keybase-gui
    libnotify
    lightum
    powertop
    rofi
    rofi-emoji
    rofi-pass
    rofi-wifi-menu
    scrot
    stalonetray
    unclutter-xfixes
    wirelesstools
    xclip xsel
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-dropbox-plugin
    xfce.thunar_volman
    xfce.xfce4_power_manager
    xorg.libXrandr
    xorg.xbacklight
    xorg.xcursorthemes
    xorg.xf86inputkeyboard
    zeal
  ]
  ++ lib.optionals stdenv.isDarwin [
    skhd
    Alfred
    Dash
    Docker
    pinentry_mac
    yabai
  ]);
}
