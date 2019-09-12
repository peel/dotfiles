{ config, pkgs, colors, ... }:

let
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

  services.yabai.enable = true;
  services.yabai.package = pkgs.yabai;
  services.yabai.config = ''
    yabai -m config mouse_follows_focus          off
    yabai -m config focus_follows_mouse          off
    yabai -m config window_placement             second_child
    yabai -m config window_topmost               off
    yabai -m config window_opacity               on
    yabai -m config window_opacity_duration      0.0
    yabai -m config window_shadow                on
    yabai -m config window_border                off
    yabai -m config active_window_opacity        1.0
    yabai -m config normal_window_opacity        0.9
    yabai -m config split_ratio                  0.62
    yabai -m config auto_balance                 off

    yabai -m config layout                       bsp
    yabai -m config top_padding                  0
    yabai -m config bottom_padding               0
    yabai -m config left_padding                 0
    yabai -m config right_padding                0
    yabai -m config window_gap                   0

    yabai -m rule --add app="emacs" role="^AXTextField$" subrole="^AXStandardWindow$" manage="on"
 '';
  services.skhd.enable = true;
  services.skhd.package =  pkgs.skhd;
  services.skhd.skhdConfig = let
    modMask = "cmd";
    moveMask = "ctrl + cmd";
    myTerminal = "emacsclient -a '' -nc --eval '(peel/vterm)'";
    myEditor = "emacsclient -a '' -nc";
    noop = "/dev/null";
    prefix = "${pkgs.yabai}/bin/yabai -m";
    fstOrSnd = {fst, snd}: domain: "${prefix} ${domain} --focus ${fst} || ${prefix} ${domain} --focus ${snd}";
    nextOrFirst = fstOrSnd { fst = "next"; snd = "first";};
    prevOrLast = fstOrSnd { fst = "prev"; snd = "last";};
  in ''
    # windows ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # select
    ${modMask} - j                            : ${prefix} window --focus next || ${prefix} window --focus "$((${prefix} query --spaces --display next || ${prefix} query --spaces --display first) |${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."first-window"')" || ${prefix} display --focus next || ${prefix} display --focus first
    ${modMask} - k                            : ${prefix} window --focus prev || ${prefix} window --focus "$((yabai -m query --spaces --display prev || ${prefix} query --spaces --display last) | ${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."last-window"')" || ${prefix} display --focus prev || ${prefix} display --focus last

    # close
    ${modMask} - ${keycodes.Delete}           : ${prefix} window --close

    # fullscreen
    ${modMask} - h                            : ${prefix} window --toggle zoom-fullscreen

    # rotate
    ${modMask} - r                            : ${prefix} space --rotate 180

    # increase region
    ${modMask} - ${keycodes.LeftBracket}      : ${prefix} window --resize left:-20:0
    ${modMask} - ${keycodes.RightBracket}     : ${prefix} window --resize right:-20:0
    
    # spaces ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # switch 
    ${modMask} + alt - j                      : ${prevOrLast "space"}
    ${modMask} + alt - k                      : ${nextOrFirst "space"}

    # send window 
    ${modMask} + ${moveMask} - j              : ${prefix} window --space prev
    ${modMask} + ${moveMask} - k              : ${prefix} window --space next

    # display  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # focus 
    ${modMask} - left                         : ${prevOrLast "display"}
    ${modMask} - right                        : ${nextOrFirst "display"}

    # send window
    ${moveMask} - right                       : ${prefix} window --display prev
    ${moveMask} - left                        : ${prefix} window --display next

    # apps  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    ${modMask} - return                       : ${myTerminal} 
    ${modMask} + shift - return               : ${myEditor}

    # reset  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    ${modMask} - q                            : pkill yabai; pkill skhd
  '';
}
