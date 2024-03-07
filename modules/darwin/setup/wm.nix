{config, pkgs, ...}:

let
  keycodes = import ./keycodes.nix;
  nav = order: operation: pkgs.writeShellScriptBin "yabai-cycle-clockwise" ''
    win=$(${pkgs.yabai} -m query --windows --window ${order} | ${pkgs.jq} '.id')
    while : ; do
        ${pkgs.yabai} -m window $win --${operation} prev &> /dev/null
        if [[ $? -eq 1 ]]; then
            break
        fi
    done
  '';
  clockwise = nav "last" "swap";
  counterClockwise = "nav" "first" "swap";
  captureTitle = "org-capture";
in {
  services.yabai.enable = true;
  services.yabai.package = pkgs.yabai;
  services.yabai.enableScriptingAddition = true;
  services.yabai.extraConfig = ''
    yabai -m config debug_output                  on
    yabai -m config mouse_follows_focus           off
    yabai -m config focus_follows_mouse           off
    yabai -m config window_placement              second_child
    yabai -m config window_topmost                off
    yabai -m config window_opacity                on
    yabai -m config window_opacity_duration       0.0
    yabai -m config window_shadow                 on
    yabai -m config window_border                 off
    yabai -m config active_window_opacity         1.0
    yabai -m config normal_window_opacity         0.85
    yabai -m config split_ratio                   0.62
    yabai -m config auto_balance                  off

    yabai -m config layout                        bsp
    yabai -m config top_padding                   0
    yabai -m config bottom_padding                0
    yabai -m config left_padding                  0
    yabai -m config right_padding                 0
    yabai -m config window_gap                    0


    yabai -m rule --add app="emacs" role="AXTextField" subrole="AXStandardWindow" manage="on"
    # grid="<rows>:<cols>:<start-x>:<start-y>:<width>:<height>"
    # yabai -m rule --add app="emacs" role="AXTextField" subrole="AXStandardWindow" title="^${captureTitle}$" manage="off" sticky="on" grid="1:2:1:1:4:4"
    yabai -m rule --add app="Synology Surveillance Station Client" sticky="on" grid="4:4:3:0:1:1"
    yabai -m rule --add app="Dash"                manage="off"
    yabai -m rule --add app="1Password"           manage="off"
    yabai -m rule --add app="System Preferences"  manage="off"
    yabai -m rule --add app="Plexamp"             manage="off"
 '';
  services.skhd.enable = true;
  services.skhd.package =  pkgs.skhd;
  services.skhd.skhdConfig = let
    modMask = "cmd";
    moveMask = "ctrl + cmd";
    myCapture = "emacsclient -c -F '((name . \"${captureTitle}\"))' --eval '(peel/org-roam-capture)'";
    myEditor = "emacsclient -a '' -nc";
    myPlayer = "open /Applications/Plexamp.app";
    noop = "/dev/null";
    prefix = "yabai -m";
    fstOrSnd = {fst, snd}: domain: "${prefix} ${domain} --focus ${fst} || ${prefix} ${domain} --focus ${snd}";
    nextOrFirst = fstOrSnd { fst = "next"; snd = "first";};
    prevOrLast = fstOrSnd { fst = "prev"; snd = "last";};
  in ''
    # windows ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    # select
    ${modMask} - j                            : ${prefix} window --focus next || ${prefix} window --focus "$((${prefix} query --spaces --display next || ${prefix} query --spaces --display first) |${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."first-window"')" || ${prefix} display --focus next || ${prefix} display --focus first
    ${modMask} - k                            : ${prefix} window --focus prev || ${prefix} window --focus "$((yabai -m query --spaces --display prev || ${prefix} query --spaces --display last) | ${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."last-window"')" || ${prefix} display --focus prev || ${prefix} display --focus last

    # close
    ${modMask} - ${keycodes.Delete}           : ${prefix} window --close && yabai -m window --focus prev

    # fullscreen
    ${modMask} - h                            : ${prefix} window --toggle zoom-fullscreen

    # rotate
    ${modMask} - r                            : ${prefix} window --focus smallest && yabai -m window --warp largest && yabai -m window --focus largest

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
    ${modMask} - return                       : ${myEditor}
    ${modMask} + shift - return               : ${myCapture}
    ${modMask} - p                            : ${myPlayer}


    # reset  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
    ${modMask} - q                            : pkill yabai; pkill skhd; osascript -e 'display notification "wm restarted"'
  '';
}
