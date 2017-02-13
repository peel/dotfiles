#!/bin/sh

if [ ! -d "$HOME/dotfiles" ]; then
    echo "Installing dotfiles for the first time"
    git clone --depth=1 https://github.com/peel/dotfiles.git "$HOME/dotfiles"
    cd "$HOME/dotfiles"
    if [ "$1" = "tailored" ]; then
        echo "How to tailor your dotfiles?\n\t1) Full install (incl desktop apps on OSX)\n\t2) Minimal install (homebrew & dotfiles)\n\t3) Link dotfiles only (requires GNU Stow)\n\t4) Nix install (NixOS & macOS)"
        read n
        case $n in
            1) echo "Full install that is." && make ;;
            2) echo "Minimal install that is." && make minimal ;;
            3) echo "Linking dotfiles only." && make link ;;
            4) echo "Nix install that is." && make nix ;;
            *) invalid option;;
        esac
    elif [ "$1" = "minimal" ]; then
        make minimal
    elif [ "$1" = "link" ]; then
        make link
    elif [ "$1" = "nix" ]; then
        make nix
    else
        make
    fi
else
    echo "Dotfiles already installed"
fi
