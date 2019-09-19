{ config, pkgs, lib, ... }:

# TODO TODO: should run both on nixos and darwin

let
  home = "$HOME/.config/weechat";
  cfg = pkgs.writeTextFile {
    name = "weechat.cfg";
    text = ''
      /set relay.network.password "''${sec.data.relaypass}"
      /server add freenode bnc.irccloud.com/6697 -ssl -username=peel -password="''${sec.data.bncfreenode}"
      /set irc.server.freenode.autojoin "#nixos,#nixos-emacs,#nix-darwin,#haskell,#chunkwm"
      /server add fp-chat slack.irccloud.com/6697 -ssl -username=peel -password="''${sec.data.bncfpchat}"
      /set irc.server.fp-chat.autojoin "nix,haskell,haskell-beginners,purescript,purescript-beginners,dhall"
      /connect -all
      /save
      /reload
    '';
  };
in {
  services.weechat = {
    enable = true;
    home = home;
    withSlack = false;
    withMatrix = false;
    extraConfigFiles = [
      ./weechat.private.nix
      cfg
    ];
  };
}
