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
      /filter add joinquit * irc_join,irc_part,irc_quit *
      /set irc.server_default.msg_part ""
      /set irc.server_default.msg_quit ""
      /set irc.ctcp.clientinfo ""
      /set irc.ctcp.finger ""
      /set irc.ctcp.source ""
      /set irc.ctcp.time ""
      /set irc.ctcp.userinfo ""
      /set irc.ctcp.version ""
      /set irc.ctcp.ping ""
      /plugin unload xfer
      /set weechat.plugin.autoload "*,!xfer"
      /connect -all
      /save
      /reload
    '';
  };
in {
}
