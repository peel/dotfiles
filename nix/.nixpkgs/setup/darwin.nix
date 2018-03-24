{ config, pkgs, colors, ... }:

let
  tmuxConfig = import ./tmux.nix {inherit colors;};
  keycodes = {
    A                    ="0x00";
    S                    ="0x01";
    D                    ="0x02";
    F                    ="0x03";
    H                    ="0x04";
    G                    ="0x05";
    Z                    ="0x06";
    X                    ="0x07";
    C                    ="0x08";
    V                    ="0x09";
    B                    ="0x0B";
    Q                    ="0x0C";
    W                    ="0x0D";
    E                    ="0x0E";
    R                    ="0x0F";
    Y                    ="0x10";
    T                    ="0x11";
    k1                    ="0x12";
    k2                    ="0x13";
    k3                    ="0x14";
    k4                    ="0x15";
    k6                    ="0x16";
    k5                    ="0x17";
    Equal                ="0x18";
    k9                    ="0x19";
    k7                    ="0x1A";
    Minus                ="0x1B";
    k8                    ="0x1C";
    k0                    ="0x1D";
    RightBracket         ="0x1E";
    O                    ="0x1F";
    U                    ="0x20";
    LeftBracket          ="0x21";
    I                    ="0x22";
    P                    ="0x23";
    L                    ="0x25";
    J                    ="0x26";
    Quote                ="0x27";
    K                    ="0x28";
    Semicolon            ="0x29";
    Backslash            ="0x2A";
    Comma                ="0x2B";
    Slash                ="0x2C";
    N                    ="0x2D";
    M                    ="0x2E";
    Period               ="0x2F";
    Grave                ="0x32";
    KeypadDecimal        ="0x41";
    KeypadMultiply       ="0x43";
    KeypadPlus           ="0x45";
    KeypadClear          ="0x47";
    KeypadDivide         ="0x4B";
    KeypadEnter          ="0x4C";
    KeypadMinus          ="0x4E";
    KeypadEquals         ="0x51";
    Keypad0              ="0x52";
    Keypad1              ="0x53";
    Keypad2              ="0x54";
    Keypad3              ="0x55";
    Keypad4              ="0x56";
    Keypad5              ="0x57";
    Keypad6              ="0x58";
    Keypad7              ="0x59";
    Keypad8              ="0x5B";
    Keypad9              ="0x5C";
    Return                    ="0x24";
    Tab                       ="0x30";
    Space                     ="0x31";
    Delete                    ="0x33";
    Escape                    ="0x35";
    Command                   ="0x37";
    Shift                     ="0x38";
    CapsLock                  ="0x39";
    Option                    ="0x3A";
    Control                   ="0x3B";
    RightCommand              ="0x36";
    RightShift                ="0x3C";
    RightOption               ="0x3D";
    RightControl              ="0x3E";
    Function                  ="0x3F";
    F17                       ="0x40";
    VolumeUp                  ="0x48";
    VolumeDown                ="0x49";
    Mute                      ="0x4A";
    F18                       ="0x4F";
    F19                       ="0x50";
    F20                       ="0x5A";
    F5                        ="0x60";
    F6                        ="0x61";
    F7                        ="0x62";
    F3                        ="0x63";
    F8                        ="0x64";
    F9                        ="0x65";
    F11                       ="0x67";
    F13                       ="0x69";
    F16                       ="0x6A";
    F14                       ="0x6B";
    F10                       ="0x6D";
    F12                       ="0x6F";
    F15                       ="0x71";
    Help                      ="0x72";
    Home                      ="0x73";
    PageUp                    ="0x74";
    ForwardDelete             ="0x75";
    F4                        ="0x76";
    End                       ="0x77";
    F2                        ="0x78";
    PageDown                  ="0x79";
    F1                        ="0x7A";
    LeftArrow                 ="0x7B";
    RightArrow                ="0x7C";
    DownArrow                 ="0x7D";
    UpArrow                   ="0x7E";
};
in {
  programs.tmux.tmuxConfig = tmuxConfig;
  programs.tmux.enableSensible = true;
  programs.tmux.enableMouse = true;
  programs.tmux.enableVim = true;
  programs.bash.enable = true;
  services.mopidy.package = "/usr/local";
  services.mopidy.enable = true;
  services.mopidy.mediakeys.package = "/usr/local";
  services.mopidy.mediakeys.enable = true;

  environment.variables.HOMEBREW_CASK_OPTS = "--appdir=/Applications/cask";
  environment.variables.TERMINFO = "/usr/share/terminfo";

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
  services.nix-daemon.enable = true;
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
  '';
  services.skhd.enable = true;
  services.skhd.package =  pkgs.skhd;
  services.skhd.skhdConfig = ''
    #  NOTE(koekeishiya): A list of all built-in modifier and literal keywords can
    #                     be found at https://github.com/koekeishiya/skhd/issues/1
    #
    #                     A hotkey is written according to the following rules:
    #
    #                       hotkey   = <keysym> ':' <command> |
    #                                  <keysym> '->' ':' <command>
    #
    #                       keysym   = <mod> '-' <key> | <key>
    #
    #                       mod      = 'built-in mod keyword' | <mod> '+' <mod>
    #
    #                       key      = <literal> | <keycode>
    #
    #                       literal  = 'single letter or built-in keyword'
    #
    #                       keycode  = 'apple keyboard kVK_<Key> values (0x3C)'
    #
    #                       ->       = keypress is not consumed by skhd
    #
    #                       command  = command is executed through '$SHELL -c' and
    #                                  follows valid shell syntax. if the $SHELL environment
    #                                  variable is not set, it will default to '/bin/bash'.
    #                                  when bash is used, the ';' delimeter can be specified
    #                                  to chain commands.
    #
    #                                  to allow a command to extend into multiple lines,
    #                                  prepend '\' at the end of the previous line.
    #
    #                                  an EOL character signifies the end of the bind.


    # restart chunkwm
    cmd + alt + ctrl - q : killall chunkwm

    # open terminal, blazingly fast compared to iTerm/Hyper
    cmd - return : ${pkgs.alacritty}/bin/alacritty

    # close focused window
    cmd - ${keycodes.Delete} : chunkc tiling::window --close

    # focus window
    alt - h : chunkc tiling::window --focus west
    alt - j : chunkc tiling::window --focus south
    alt - k : chunkc tiling::window --focus north
    alt - l : chunkc tiling::window --focus east

    cmd - j : chunkc tiling::window --focus prev
    cmd - k : chunkc tiling::window --focus next

    # equalize size of windows
    shift + alt - 0 : chunkc tiling::desktop --equalize

    # swap window
    shift + alt - h : chunkc tiling::window --swap west
    shift + alt - j : chunkc tiling::window --swap south
    shift + alt - k : chunkc tiling::window --swap north
    shift + alt - l : chunkc tiling::window --swap east

    # move window
    shift + cmd - h : chunkc tiling::window --warp west
    shift + cmd - j : chunkc tiling::window --warp south
    shift + cmd - k : chunkc tiling::window --warp north
    shift + cmd - l : chunkc tiling::window --warp east

    # send window to desktop
    shift + alt - x : chunkc tiling::window --send-to-desktop $(chunkc get _last_active_desktop)
    shift + alt - z : chunkc tiling::window --send-to-desktop prev
    shift + alt - c : chunkc tiling::window --send-to-desktop next

    # focus monitor
    cmd + left  : chunkc tiling::monitor -f prev
    cmd + right  : chunkc tiling::monitor -f next
    ctrl + alt - 1  : chunkc tiling::monitor -f 1
    ctrl + alt - 2  : chunkc tiling::monitor -f 2
    ctrl + alt - 3  : chunkc tiling::monitor -f 3

    # send window to monitor and follow focus
    ctrl + cmd - z  : chunkc tiling::window --send-to-monitor prev; chunkc tiling::monitor -f prev
    ctrl + cmd - c  : chunkc tiling::window --send-to-monitor next; chunkc tiling::monitor -f next
    ctrl + cmd - 1  : chunkc tiling::window --send-to-monitor 1; chunkc tiling::monitor -f 1
    ctrl + cmd - 2  : chunkc tiling::window --send-to-monitor 2; chunkc tiling::monitor -f 2
    ctrl + cmd - 3  : chunkc tiling::window --send-to-monitor 3; chunkc tiling::monitor -f 3

    # increase region size
    shift + alt - a : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge west
    shift + alt - s : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge south
    shift + alt - w : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge north
    shift + alt - d : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge east

    # decrease region size
    shift + cmd - a : chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge west
    shift + cmd - s : chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge south
    shift + cmd - w : chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge north
    shift + cmd - d : chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge east

    # set insertion point for focused container
    ctrl + alt - f : chunkc tiling::window --use-insertion-point cancel
    ctrl + alt - h : chunkc tiling::window --use-insertion-point west
    ctrl + alt - j : chunkc tiling::window --use-insertion-point south
    ctrl + alt - k : chunkc tiling::window --use-insertion-point north
    ctrl + alt - l : chunkc tiling::window --use-insertion-point east

    # rotate tree
    alt - r : chunkc tiling::desktop --rotate 90

    # mirror tree y-axis
    alt - y : chunkc tiling::desktop --mirror vertical

    # mirror tree x-axis
    alt - x : chunkc tiling::desktop --mirror horizontal

    # toggle desktop offset
    alt - a : chunkc tiling::desktop --toggle offset

    # toggle window fullscreen
    alt - f : chunkc tiling::window --toggle fullscreen

    # toggle window native fullscreen
    shift + alt - f : chunkc tiling::window --toggle native-fullscreen

    # toggle window parent zoom
    alt - d : chunkc tiling::window --toggle parent

    # toggle window split type
    alt - e : chunkc tiling::window --toggle split

    # float / unfloat window and center on screen
    alt - t : chunkc tiling::window --toggle float;\
              chunkc tiling::window --grid-layout 4:4:1:1:2:2

    # toggle sticky, float and resize to picture-in-picture size
    alt - s : chunkc tiling::window --toggle sticky;\
              chunkc tiling::window --grid-layout 5:5:4:0:1:1

    # float next window to be tiled
    shift + alt - t : chunkc set window_float_next 1

    # change layout of desktop
    ctrl + alt - a : chunkc tiling::desktop --layout bsp
    ctrl + alt - s : chunkc tiling::desktop --layout monocle
    ctrl + alt - d : chunkc tiling::desktop --layout float

    ctrl + alt - w : chunkc tiling::desktop --deserialize ~/.chunkwm_layouts/dev_1
  '';
}
