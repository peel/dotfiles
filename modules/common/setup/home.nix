{config, pkgs, pkgs-unstable, stdenv, lib, ...}:

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
    programs.tmux = {
      enable = true;
      prefix = "C-a";
      clock24 = true;
      disableConfirmationPrompt = true;
      focusEvents = true;
      historyLimit = 5000;
      keyMode = "emacs";
      mouse = true;
      plugins = [
      ];
      sensibleOnTop = true;
      tmuxinator.enable = true;
      tmuxp.enable = true;
    };
    home.sessionVariables.COLORTERM = "truecolor";
    home.sessionVariables.BASH_ENV = "$HOME/.bashenv";
    home.file.".bashenv".text = ''
      if command -v direnv &>/dev/null; then
        export DIRENV_LOG_FORMAT=""
        eval "$(direnv export bash 2>/dev/null)"
      fi
    '';
    programs.ghostty = {
      enable = true;
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs-unstable.ghostty-bin else pkgs-unstable.ghostty;
      settings = {
        font-family = "PragmataPro";
        font-size = 22;
        font-thicken = true;
        font-thicken-strength = 0;
        adjust-cell-height = "50%";
        mouse-shift-capture = "never";
        shell-integration = "detect";
        shell-integration-features = "cursor,sudo,title";
        theme = "light:solo-jazz,dark:gotham";
        cursor-style = "block";
        cursor-style-blink = false;
        scrollback-limit = 1000000;
        window-inherit-working-directory = true;
        window-inherit-font-size = true;
        confirm-close-surface = false;
        window-padding-x = 20;
        window-padding-y = 5;
        window-padding-color = "extend";
        window-vsync = true;
        term = "ghostty";
        clipboard-read = "allow";
        clipboard-write = "allow";
        desktop-notifications = true;
        macos-option-as-alt = true;
        macos-titlebar-style = "tabs";
        quick-terminal-position = "top";
        quick-terminal-screen = "main";
        quick-terminal-animation-duration = 0.2;
        quick-terminal-autohide = true;
        keybind = [
          "super+shift+k=toggle_quick_terminal"
          "super+shift+v=toggle_command_palette"
          "shift+enter=text:\\x1b\\r"
        ];
      };
      themes = {
        gotham = {
          background = "0c1014";
          foreground = "99d1ce";
          cursor-color = "599cab";
          selection-background = "091f2e";
          selection-foreground = "d3ebe9";
          palette = [
            "0=#0c1014"
            "1=#c23127"
            "2=#2aa889"
            "3=#edb443"
            "4=#195466"
            "5=#4e5166"
            "6=#33859e"
            "7=#99d1ce"
            "8=#11151c"
            "9=#d26937"
            "10=#091f2e"
            "11=#245361"
            "12=#0a3749"
            "13=#888ca6"
            "14=#599cab"
            "15=#d3ebe9"
          ];
        };
        solo-jazz = {
          background = "fafafa";
          foreground = "3b2685";
          cursor-color = "04c4c7";
          cursor-text = "fafafa";
          selection-background = "eaeafa";
          selection-foreground = "3b2685";
          palette = [
            "0=#24292e"
            "1=#fe2500"
            "2=#10d7ae"
            "3=#ff5200"
            "4=#005cc5"
            "5=#c71585"
            "6=#009c9f"
            "7=#d0d0d0"
            "8=#778ca3"
            "9=#fa1090"
            "10=#10d7ae"
            "11=#ff5200"
            "12=#00afef"
            "13=#9d2dab"
            "14=#04c4c7"
            "15=#fafafa"
          ];
        };
      };
    };
    programs.alacritty = {
      enable = true;
      theme = "gotham";
      settings = {
        env = {
          TERM = "xterm-256color";
          COLORTERM = "truecolor";
        };
        cursor.style.blinking = "Never";
        keyboard.bindings = [
          { key = "Slash"; mods = "Control"; chars = "\\u001f"; }
        ];
        window = {
          option_as_alt = "Both";
          decorations = "buttonless";
          padding = {
            x = 10;
            y = 10;
          };
          dynamic_padding = true;
        };
        mouse.hide_when_typing = true;
        font = {
          size = 23;
          offset.y = 10;
          normal = {
            family = "PragmataPro";
            style = "Regular";
          };
        };
      };
    };
    home.packages = [
      pkgs.awscli
      pkgs._1password-cli
      pkgs.docker
      pkgs.jq
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
