{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    coreutils
    gnumake
    curl
    emacs
    nox
    fasd
    fish
    git
    gitAndTools.hub
    stow
    nix-repl
    tmux
    less
    gawk
    parallel
    fasd
    silver-searcher
    ranger
    httpie
    jq
    xquartz
    kwm
    khd
    emacs
    aspell
    diff-so-fancy
    gist
    hub
    awscli
    kubernetes
    virtualbox
    vagrant
    openjdk
    sbt
    scala
    scalafmt
    erlang
    elixir
    elmPackages.elm
    nodejs
    mycli
    ngrok
    asciinema
    wakatime
    calibre
    transmission
    vlc
    spotify
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