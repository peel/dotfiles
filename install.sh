#!/bin/sh

if [ ! -d "$HOME/wrk/dotfiles" ]; then
    echo "Installing Dotfils for the first time"
    git clone --depth=1 https://github.com/peel/dotfiles.git "$HOME/wrk/dotfiles"
    cd "$HOME/wrk/dotfiles"
    make
else
    echo "Dotfiles already installed"
fi
