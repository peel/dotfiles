# global env
set LC_ALL en_US.UTF-8
set LANG en_US.UTF-8
set -x TERMINFO /usr/share/terminfo
set -x SHELL (which fish)
set -x EDITOR "emacsclient -tc"
set -U fish $HOME/.config/fish
set -x fish_greeting ''

# key bindings
set -g fish_key_bindings fish_vi_key_bindings

# aliases
each 'source $_1' (ls $fish/aliases/*)

set PATH $PATH $HOME/.bin/

# nix
if test -e $HOME/.nix-profile
  set -x PATH $PATH $HOME/.nix-profile/bin /run/current-system/sw/bin
  set -x NIX_PATH darwin=/Users/peel/.nix-defexpr/darwin:darwin-config=/Users/peel/.nixpkgs/darwin-configuration.nix:nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels
  set -x SSL_CERT_FILE $HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt
end

# homebrew
if test -e /usr/local/bin/brew
  set HOMEBREW_NO_ANALYTICS 1
  set HOMEBREW_CASK_OPTS "--appdir=~/Applications --caskroom=/usr/local/Caskroom"
end
