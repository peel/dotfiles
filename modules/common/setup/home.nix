{config, pkgs, stdenv, lib, ...}:

let
  common = {
    programs.home-manager.enable = true;
    home.stateVersion = "22.11"; 
    manual.manpages.enable = false;
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };
    home.packages = [
      pkgs.mpv
      pkgs.awscli
      pkgs._1password
    ];
  };
  nixos = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    home.sessionVariables = {
      GDK_SCALE = "2";
      QT_QPA_PLATFORM = "wayland";
      SDL_VIDEODRIVER = "wayland";
      XDG_SESSION_TYPE = "wayland";
    };
    home.packages = [ pkgs.wofi pkgs._1password-gui ];
    # FIXME
    # (23.05) migrate to services.clipman.enable = true;
    systemd.user.services.clipman = {
      Unit = {
        Description = "Clipboard management daemon";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart =
          "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch ${pkgs.clipman}/bin/clipman store";
        ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR2 $MAINPID";
        Restart = "on-failure";
        KillMode = "mixed";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
    wayland.windowManager.sway = rec {
      enable = true;
      extraOptions = [ "--unsupported-gpu" ];
      wrapperFeatures.gtk = true;
      config = rec {
        modifier = "Mod1";
        terminal = "emacsclient -a '' -c --eval '(vterm)'";
        menu = "wofi";
        startup = [
          {command = "dbus-update-activation-environment --systemd WAYLAND_DISPLAY DISPLAY";}
        ];
        keybindings =
          let modMask = config.modifier;
          in lib.mkOptionDefault {
            "${modMask}+Return" = "exec emacsclient -c";
            "${modMask}+space" = "exec wofi --show=run";
          };
        output = {
          Virtual-1 = {
            res = "--custom 3418x2234";
            scale = "2.6";
            bg = "/home/peel/wrk/bg/johnson-wang-iI4sR_nkkbc-unsplash.jpg fill";
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
