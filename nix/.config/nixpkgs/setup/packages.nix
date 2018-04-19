{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; ([
    alacrittyWrapper
    aspell
    awscli
    curl
    coreutils
    docker
    docker_compose
    emacs
    fasd
    fish
    fzf
    gist
    gitAndTools.hub
    gitFull
    gnumake
    gnupg
    gopass
    ix
    # graphviz
    # hoverfly
    httpie
    jq
    nixUnstable
    # purescript
    qarma
    ranger
    ripgrep
    sbt
    scripts
    stow
    wakatime
  ]
  ++ lib.optionals stdenv.isLinux [
    acpi
    arandr
    autorandr
    blueman
    bluez
    cabal2nix
    dunst
    feh
    firefox
    haskellPackages.cabal-install
    haskellPackages.ghc
    # haskellPackages.ghcWithHoogle
    # haskellPackages.stack
    haskellPackages.apply-refact
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.hasktags
    # haskellPackages.ghc-mod
    # haskellPackages.intero
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.yeganesh
    htop
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
    spotify
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
    chunkwm.border
    chunkwm.core
    chunkwm.ffm
    chunkwm.tiling
    firefox-bin
    skhd
  ]);
}
