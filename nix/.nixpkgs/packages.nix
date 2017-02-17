{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    asciinema
    aspell
    #calibre
    coreutils
    curl
    ##diff-so-fancy
    #elixir
    #elmPackages.elm
    #emacs
    #erlang
    fasd
    fish
    gawk
    gist
    git
    gitAndTools.hub
    gnumake
    httpie
    jq
    khd
    #kubernetes
    kwm
    less
    ##mycli
    ngrok
    nix-repl
    #nodejs
    #nox
    #openjdk
    parallel
    ranger
    #sbt
    #scala
    #scalafmt
    silver-searcher
    ##spotify
    stow
    tmux
    #transmission
    #vagrant
    #virtualbox
    #vlc
    wakatime
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