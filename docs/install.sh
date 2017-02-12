#!/bin/sh

if [ ! -d "$HOME/dotfiles" ]; then
    echo "Installing Dotfils for the first time"
    git clone --depth=1 https://github.com/peel/dotfiles.git "$HOME/wrk/dotfiles"
    cd "$HOME/dotfiles"
    if [ "$1" = "minimal" ]; then
        make minimal
    else
        make
    fi
else
    echo "Dotfiles already installed"
fi
