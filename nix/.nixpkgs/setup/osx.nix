{ config, pkgs, ... }:

{
  system.defaults = {
    dock = {
      autohide = true;
      orientation = "bottom";
      showhidden = true;
    };
    finder = {
      AppleShowAllExtensions = true;
      FXEnableExtensionChangeWarning = false;
    };
  };
  services.activate-system.enable = true;
  services.nix-daemon.enable = false;
  services.chunkwm.enable = true;
  services.khd.enable = true;
  services.khd.khdConfig = ''
    # restart chunkwm
    cmd + alt + ctrl - q : killall chunkwm

    # reload config
    cmd + alt + ctrl - r : khd -e "reload"

    # open terminal, blazingly fast compared to iTerm/Hyper
    cmd - return : open -na ~/Applications/iTerm.app

    # focus window
    alt - h : chunkc window --focus west
    alt - j : chunkc window --focus south
    alt - k : chunkc window --focus north
    alt - l : chunkc window --focus east

    # equalize size of windows
    shift + alt - 0 : chunkc desktop --equalize

    # swap window
    shift + alt - h : chunkc window --swap west
    shift + alt - j : chunkc window --swap south
    shift + alt - k : chunkc window --swap north
    shift + alt - l : chunkc window --swap east

    # move window
    shift + cmd - h : chunkc window --warp west
    shift + cmd - j : chunkc window --warp south
    shift + cmd - k : chunkc window --warp north
    shift + cmd - l : chunkc window --warp east

    # move floating windows / windows on a floating space
    shift + alt - up     : chunkc window --warp-floating fullscreen
    shift + alt - left   : chunkc window --warp-floating left
    shift + alt - right  : chunkc window --warp-floating right
    shift + cmd - left   : chunkc window --warp-floating top-left
    shift + cmd - right  : chunkc window --warp-floating top-right
    shift + ctrl - left  : chunkc window --warp-floating bottom-left
    shift + ctrl - right : chunkc window --warp-floating bottom-right

    # send window to desktop
    shift + alt - x : chunkc window --send-to-desktop $(chunkc query _last_active_desktop)
    shift + alt - z : chunkc window --send-to-desktop prev
    shift + alt - c : chunkc window --send-to-desktop next
    shift + alt - 1 : chunkc window --send-to-desktop 1
    shift + alt - 2 : chunkc window --send-to-desktop 2
    shift + alt - 3 : chunkc window --send-to-desktop 3
    shift + alt - 4 : chunkc window --send-to-desktop 4
    shift + alt - 5 : chunkc window --send-to-desktop 5
    shift + alt - 6 : chunkc window --send-to-desktop 6

    # send window to desktop and switch desktop
    shift + cmd - x : `id=$(chunkc query _last_active_desktop); chunkc window -d $id; khd -p "cmd + alt - $id" &> /dev/null`
    shift + cmd - z : chunkc window -d prev; khd -p "cmd + alt - z"
    shift + cmd - c : chunkc window -d next; khd -p "cmd + alt - c"
    shift + cmd - 1 : chunkc window -d 1; khd -p "cmd + alt - 1"
    shift + cmd - 2 : chunkc window -d 2; khd -p "cmd + alt - 2"
    shift + cmd - 3 : chunkc window -d 3; khd -p "cmd + alt - 3"
    shift + cmd - 4 : chunkc window -d 4; khd -p "cmd + alt - 4"
    shift + cmd - 5 : chunkc window -d 5; khd -p "cmd + alt - 5"
    shift + cmd - 6 : chunkc window -d 6; khd -p "cmd + alt - 6"

    # switch to last active desktop
    cmd + alt - x   : `id=$(chunkc query _last_active_desktop); khd -p "cmd + alt - $id" &> /dev/null`

    # focus monitor
    ctrl + alt - z  : chunkc monitor -f prev
    ctrl + alt - c  : chunkc monitor -f next
    ctrl + alt - 1  : chunkc monitor -f 1
    ctrl + alt - 2  : chunkc monitor -f 2
    ctrl + alt - 3  : chunkc monitor -f 3

    # send window to monitor and follow focus
    ctrl + cmd - z  : chunkc window --send-to-monitor prev; chunkc monitor -f prev
    ctrl + cmd - c  : chunkc window --send-to-monitor next; chunkc monitor -f next
    ctrl + cmd - 1  : chunkc window --send-to-monitor 1; chunkc monitor -f 1
    ctrl + cmd - 2  : chunkc window --send-to-monitor 2; chunkc monitor -f 2
    ctrl + cmd - 3  : chunkc window --send-to-monitor 3; chunkc monitor -f 3

    # increase region size
    shift + alt - a : chunkc window --use-temporary-ratio 0.1 --adjust-window-edge west
    shift + alt - s : chunkc window --use-temporary-ratio 0.1 --adjust-window-edge south
    shift + alt - w : chunkc window --use-temporary-ratio 0.1 --adjust-window-edge north
    shift + alt - d : chunkc window --use-temporary-ratio 0.1 --adjust-window-edge east

    # decrease region size
    shift + cmd - a : chunkc window --use-temporary-ratio -0.1 --adjust-window-edge west
    shift + cmd - s : chunkc window --use-temporary-ratio -0.1 --adjust-window-edge south
    shift + cmd - w : chunkc window --use-temporary-ratio -0.1 --adjust-window-edge north
    shift + cmd - d : chunkc window --use-temporary-ratio -0.1 --adjust-window-edge east

    # set insertion point
    ctrl + alt - f : chunkc window --use-insertion-point focus
    ctrl + alt - h : chunkc window --use-insertion-point west
    ctrl + alt - j : chunkc window --use-insertion-point south
    ctrl + alt - k : chunkc window --use-insertion-point north
    ctrl + alt - l : chunkc window --use-insertion-point east

    # rotate tree
    alt - r : chunkc desktop --rotate 90

    # mirror tree y-axis
    alt - y : chunkc desktop --mirror vertical

    # mirror tree x-axis
    alt - x : chunkc desktop --mirror horizontal

    # toggle desktop offset
    alt - a : chunkc desktop --toggle offset

    # toggle window fullscreen
    alt - f : chunkc window --toggle fullscreen

    # toggle window native fullscreen
    shift + alt - f : chunkc window --toggle native-fullscreen

    # toggle window parent zoom
    alt - d : chunkc window --toggle parent

    # toggle window split type
    alt - e : chunkc window --toggle split

    # float / unfloat window
    alt - t : chunkc window --toggle float

    # float next window to be tiled
    shift + alt - t : chunkc config window_float_next 1

    # change layout of desktop
    ctrl + alt - a : chunkc desktop --layout bsp
    ctrl + alt - s : chunkc desktop --layout monocle
    ctrl + alt - d : chunkc desktop --layout float

    ctrl + alt - w : chunkc desktop --deserialize ~/.chunkwm_layouts/dev_1

    # remap caps-lock to escape for this config only !!!
    # macos sierra can also perform this remap for a given keyboard
    - capslock : khd -p "- escape"

    # key remap for norwegian layout \ { }
    cmd - 7 : khd -p "shift + alt - 7"
    cmd - 8 : khd -p "shift + alt - 8"
    cmd - 9 : khd -p "shift + alt - 9"
  '';
  launchd.user.agents.fetch-nixpkgs = {
    command = "${pkgs.git}/bin/git -C ~/.nix-defexpr/nixpkgs fetch origin master";
    environment.GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    serviceConfig.KeepAlive = false;
    serviceConfig.ProcessType = "Background";
    serviceConfig.StartInterval = 60;
  };
}
