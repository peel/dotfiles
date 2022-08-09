{config, pkgs, stdenv, lib, ...}:

let
  common = {
    programs.direnv = {
      enable = true;
      # does not work, we're overriding through system-module
      enableBashIntegration = true;
      nix-direnv.enable = true;
      nix-direnv.enableFlakes = true;
    };
  #     # programs.mbsync.enable = true;
  #     # programs.msmtp.enable = true;
  #     # programs.notmuch = {
  #     #   enable = true;
  #     #   hooks = {
  #     #     preNew = "mbsync --all";
  #     #   };
  #     # };
  #     # accounts.email = {
  #     #   accounts.protonmail = {
  #     #     address = "plimanowski@pm.me";
  #     #     gpg = {
  #     #       key = "A03BCA31003F49A1D839391EF50AA5EE52A54A46";
  #     #       signByDefault = true;
  #     #     };
  #     #     imap = {
  #     #       host = "127.0.0.1";
  #     #       port = 1143;
  #     #     };
  #     #     smtp = {
  #     #       host = "127.0.0.1";
  #     #       port = 1025;
  #     #     };
  #     #     mbsync = {
  #     #       enable = true;
  #     #       create = "maildir";
  #     #     };
  #     #     msmtp.enable = true;
  #     #     notmuch.enable = true;
  #     #     primary = true;
  #     #     realName = "Piotr Limanowski";
  #     #     signature = {
  #     #       text = ''
  #     #       Piotr Peel Limanowski
  #     #       https://keybase.io/peel
  #     #     '';
  #     #       showSignature = "append";
  #     #     };
  #     #     passwordCommand = "${config.peel.secrets.protonmail-local}";
  #     #     userName = "plimanowski@pm.me";
  #     #   };
  #     };
  };
  nixos = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    programs.rofi = {
      enable = true;
    };
    services.dunst = {
      enable = true;
    };
    services.polybar = {
      enable = true;
      script = ''
        polybar top &
      '';
    };
    xsession = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp: [
          hp.dbus
          hp.monad-logger
          hp.xmonad-contrib
        ];
        config = ./config.hs;
      };
    };
  };
  darwin = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {};
in
lib.mkMerge [common nixos darwin]
