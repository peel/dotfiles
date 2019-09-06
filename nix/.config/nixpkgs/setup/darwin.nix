{ config, pkgs, colors, ... }:

let
  tmuxConfig = import ./tmux.nix {inherit colors; inherit (pkgs) tmux-prompt; };
  keycodes = import ./keycodes.nix;
in {
  nix.extraOptions = ''
      builders = @/etc/nix/machines
  '';
  system.defaults = {
    dock = {
      autohide = true;
      orientation = "right";
      showhidden = true;
      mineffect = "scale";
      launchanim = false;
      show-process-indicators = true;
      tilesize = 48;
      static-only = true;
      mru-spaces = false;
    };
    finder = {
      AppleShowAllExtensions = true;
      FXEnableExtensionChangeWarning = false;
    };
    trackpad = {
      Clicking = true;
      TrackpadThreeFingerDrag = true;
    };
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      ApplePressAndHoldEnabled = false;
      InitialKeyRepeat = 10;
      KeyRepeat = 1;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };
  };
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  networking.knownNetworkServices = ["Wi-Fi" "Bluetooth PAN" "Thunderbolt Bridge"];
  networking.dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];

  system.activationScripts.extraUserActivation.text = ''
    ln -sfn /etc/static/gitconfig $HOME/.gitconfig
    ln -sfn /etc/static/gitignore $HOME/.gitignore
  '';

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  services.bloop.enable = true;
  launchd.user.agents.chwm-sa = {
    command = "${pkgs.chunkwm.core}/bin/chunkwm --load-sa";
    serviceConfig.KeepAlive = false;
    serviceConfig.ProcessType = "Background";
    serviceConfig.RunAtLoad = true;
  };
  services.chunkwm.enable = true;
  services.chunkwm.plugins.list = [ "ffm" "tiling" ];
  services.chunkwm.package = pkgs.chunkwm.core;
  services.chunkwm.plugins.dir = "/run/current-system/sw/bin/chunkwm-plugins/";
  services.chunkwm.plugins."tiling".config = ''
    chunkc set desktop_padding_step_size     0
    chunkc set desktop_gap_step_size         0
    chunkc set global_desktop_offset_top     0
    chunkc set global_desktop_offset_bottom  0
    chunkc set global_desktop_offset_left    0
    chunkc set global_desktop_offset_right   0
    chunkc set global_desktop_offset_gap     0
    chunkc set bsp_spawn_left                1
    chunkc set bsp_optimal_ratio             1.618
    chunkc set bsp_split_mode                optimal
    chunkc set bsp_split_ratio               0.66
    chunkc set window_focus_cycle            all
    chunkc set mouse_follows_focus           1
    chunkc set window_region_locked          1

    # chwm-sa additions
    # https://github.com/koekeishiya/chwm-sa
    # warn: triggered via an impure service org.nixos.chwm-sa
    chunkc set window_float_topmost          1
    chunkc set window_fade_inactive          1
    chunkc set window_fade_alpha             0.7
    chunkc set window_fade_duration          0.1
    chunkc set window_use_cgs_move           1
  '';
  services.chunkwm.extraConfig = ''
    chunkc tiling::rule --owner "emacs.*" --except "^$" --state tile
    chunkc tiling::rule --owner "Emacs.*" --except "^$" --state tile
    chunkc tiling::rule --owner Dash --state float
  '';
  services.skhd.enable = true;
  services.skhd.package =  pkgs.skhd;
  services.skhd.skhdConfig = let
    modMask = "cmd";
    moveMask = "ctrl + cmd";
    myTerminal = "emacsclient -a '' -nc --eval '(peel/vterm)'";
    myEditor = "emacsclient -a '' -nc";
    noop = "/dev/null";
  in ''
    # windows ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # select
    ${modMask} - j                        : chunkc tiling::window --focus prev 
    ${modMask} - k                        : chunkc tiling::window --focus next

    # close
    ${modMask} - ${keycodes.Delete}       : chunkc tiling::window --close

    # fullscreen
    ${modMask} - h                        : chunkc tiling::window --toggle fullscreen

    # equalize 
    ${modMask} - 0                        : chunkc tiling::desktop --equalize

    # swap 
    ${moveMask} - h                       : chunkc tiling::desktop --mirror horizontal
    ${moveMask} - v                       : chunkc tiling::desktop --mirror vertical

    # rotate
    ${modMask} - r                        : chunkc tiling::desktop --rotate 90

    # increase region
    ${modMask} - ${keycodes.LeftBracket}  : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge west; chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge east;
    ${modMask} - ${keycodes.RightBracket} : chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge west; chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge east;

    # spaces ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # switch 
    ${modMask} + alt - ${keycodes.A}          : chunkc tiling::desktop --focus prev
    ${modMask} + alt - ${keycodes.S}          : chunkc tiling::desktop --focus next

    # send window 
    ${modMask} + ${moveMask} - ${keycodes.A}  : chunkc tiling::window --send-to-desktop prev
    ${modMask} + ${moveMask} - ${keycodes.S}  : chunkc tiling::window --send-to-desktop next

    # monitor  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # focus 
    ${modMask} - left                     : chunkc tiling::monitor -f prev
    ${modMask} - right                    : chunkc tiling::monitor -f next

    # send window
    ${moveMask} - right                   : chunkc tiling::window --send-to-monitor 1; chunkc tiling::monitor -f 1
    ${moveMask} - left                    : chunkc tiling::window --send-to-monitor 2; chunkc tiling::monitor -f 2

    # apps  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    ${modMask} - return                  : ${myTerminal} 
    ${modMask} + shift - return          : ${myEditor}
    alt - ${keycodes.Equal}              : ${pkgs.scripts}/bin/qmk $HOME/wrk/qmk_firmware/layouts/community/ortho_4x12/peel/keymap.c

    # reset  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    ${modMask} - q                       : pkill chunkwm; pkill skhd
  '';
}
