#!/bin/sh

if [ ! -d "$HOME/dotfiles" ]; then
    echo "Installing Dotfils for the first time"
    git clone --depth=1 https://github.com/peel/dotfiles.git "$HOME/dotfiles"
    cd "$HOME/dotfiles"
    if [ "$1" = "minimal" ]; then
        make minimal
    if [ "$1" = "dotfiles" ]; then
        make link
    elif [ "$1" = "nix" ]; then
        make nix
    else
        make
    fi
else
    echo "Dotfiles already installed"
fi
