{ config, pkgs, ... }:

# TODO should run both on nixos and darwin

let
  home = "$HOME/.config/weechat";
in {
  services.weechat = {
    enable = true;
    home = home;
    withSlack = false;
    withMatrix = false;
    extraConfig = import ./weechat.private.nix + ''
	     /connect -all
	     /relay add weechat 9001
	     /set relay.network.password \''${sec.data.relaypass}
	     /save
    '';
  };
}
