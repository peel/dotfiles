{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    autojump
    aspell
    #calibre
    coreutils
    curl
    ##diff-so-fancy
    #elixir
    #elmPackages.elm
    remacs
    #erlang
    fasd
    firefox-bin
    fish
    gawk
    gist
    git
    gitAndTools.hub
    graphviz
    gnumake
    httpie
    hoverfly
    jq
    chunkwm.core
    chunkwm.ffm
    chunkwm.border
    chunkwm.tiling
    chunkwm.transparency
    khd
    less
    #pythonPackages.mycli
    ngrok
    nix-repl
    #nodejs
    #nox
    #openjdk
    parallel
    purescript
    ranger
    #sbt
    #scala
    #scalafmt
    silver-searcher
    ##spotify
    stow
    #tmux #loaded by a service
    #transmission
    #vagrant
    #virtualbox
    #vlc
    wakatime
    weechat
    #xquartz
  ];
}

# GUI:
# boom
# send-to-kindle
# qnapi
# spotify
# Airmail 3
# Slack
# Tweetbot
# Irvue
# Deckset
# Pixelmator
# Affinity Designer
# rubitrack-pro
# garmin-express
# Parcel
# ubersicht
# alfred
# flux
# istat-menus
# PopClip
# iterm2
# ravenac95/sudolikeaboss/sudolikeaboss
# tldr
# dropbox
# The Unarchiver
# iridium-extra
# airvpn
# dash
# karabiner-elements
# razer-synapse
# awscli
# concourse/tapfly
# CCMenu
# ammonite-repl
# scalariform
# scalastyle
