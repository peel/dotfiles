#!/bin/sh

if ! command -v nix-env >/dev/null 2>&1; then
    nix_installer=$(mktemp)
    curl -s https://nixos.org/nix/install > $nix_installer
    sh $nix_installer
fi

nix build -f https://github.com/peel/dotfiles/archive/master.tar.gz
./result/bin/dotfiles
