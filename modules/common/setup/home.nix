{config, pkgs, stdenv, lib, ...}:

let
  common = {
    programs.home-manager.enable = true;
    home.stateVersion = "22.11"; 
    manual.manpages.enable = false;
    programs.direnv = {
      enable = true;
      # does not work, we're overriding through system-module
      enableBashIntegration = true;
      nix-direnv.enable = true;
      # nix-direnv.enableFlakes = true;
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
    home.sessionVariables = {
      GDK_SCALE = "2";
      QT_QPA_PLATFORM = "wayland";
      SDL_VIDEODRIVER = "wayland";
      XDG_SESSION_TYPE = "wayland";
    };
    wayland.windowManager.sway = rec {
      enable = true;
      extraOptions = [ "--unsupported-gpu" ];
      wrapperFeatures.gtk = true;
      config = rec {
        modifier = "Mod1";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        menu = "${pkgs.wofi}/bin/wofi";
        startup = [
          {command = "dbus-update-activation-environment --systemd WAYLAND_DISPLAY DISPLAY";}
        ];
        keybindings =
          let modMask = config.modifier;
          in lib.mkOptionDefault {
            "${modMask}+Return" = "exec emacsclient -nc";
          };
        output = {
          Virtual-1 = {
            res = "3840x2160";
          };
        };
        input = {
          "type:pointer" = {
            accel_profile = "flat";
            pointer_accel = "0";
          };
          "type:touchpad" = {
            middle_emulation = "enabled";
            natural_scroll = "enabled";
            tap = "enabled";
          };
        };
      };
      extraConfig = ''
        exec ${pkgs.xorg.xprop}/bin/xprop -root -f _XWAYLAND_GLOBAL_OUTPUT_SCALE 32c -set _XWAYLAND_GLOBAL_OUTPUT_SCALE 2
      '';
    };
  };
  darwin = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {};
in
lib.mkMerge [common nixos darwin]
