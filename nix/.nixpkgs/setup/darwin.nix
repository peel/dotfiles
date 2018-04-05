{ config, pkgs, colors, ... }:

let
  tmuxConfig = import ./tmux.nix {inherit colors; inherit (pkgs) tmux-prompt;};
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
  programs.tmux.enableFzf = true;
  programs.bash.enable = true;

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
  services.chunkwm.extraConfig = ''
    chunkc tiling::rule --owner Emacs --state tile
    chunkc tiling::rule --owner Emacs.* --state tile
    chunkc tiling::rule --owner .*Emacs --state tile
    chunkc tiling::rule --owner .*Emacs --state tile
    chunkc tiling::rule --owner \"\.qarma-wrapped\" --state native-fullscreen
  '';
  services.skhd.enable = true;
  services.skhd.package =  pkgs.skhd;
  services.skhd.skhdConfig = let
    modMask = "alt";
    myTerminal = "${pkgs.alacrittyWrapper}/bin/alacritty -e tmux -2 new-session -A -s main";
    myEditor = "${pkgs.scripts}/bin/em";
    cheatsheet = file: "${pkgs.qarma}/bin/qarma --text-info --font=PragmataPro --width 1500 --height 1500 --filename ${file}";
  in ''
    ${modMask} - q : killall chunkwm
    cmd - q : /dev/null
    cmd - h : /dev/null

    # TODO extract to make sure commands exist
    ${modMask} - return : ${myTerminal}
    ${modMask} + shift - return : ${myEditor}
    ${modMask} - ${keycodes.Equal} : ${pkgs.scripts}/bin/qmk $HOME/wrk/qmk_firmware/layouts/community/ortho_4x12/peel/keymap.c
    ${modMask} - ${keycodes.Comma} : ${cheatsheet "/etc/static/skhdrc"}

    # close focused window
    ${modMask} - ${keycodes.Delete} : chunkc tiling::window --close

    # focus window
    ${modMask} - h : chunkc tiling::window --focus west
    ${modMask} - j : chunkc tiling::window --focus south
    ${modMask} - k : chunkc tiling::window --focus north
    ${modMask} - l : chunkc tiling::window --focus east

    cmd - j : chunkc tiling::window --focus prev
    cmd - k : chunkc tiling::window --focus next

    # equalize size of windows
    shift + ${modMask} - 0 : chunkc tiling::desktop --equalize

    # swap window
    shift + ${modMask} - h : chunkc tiling::window --swap west
    shift + ${modMask} - j : chunkc tiling::window --swap south
    shift + ${modMask} - k : chunkc tiling::window --swap north
    shift + ${modMask} - l : chunkc tiling::window --swap east

    # send window to desktop
    ctrl + ${modMask} - x : chunkc tiling::window --send-to-desktop $(chunkc get _last_active_desktop)

    # focus monitor
    ${modMask} - left : chunkc tiling::monitor -f prev
    ${modMask} - right : chunkc tiling::monitor -f next
    ${modMask} - 1  : chunkc tiling::monitor -f 1
    ${modMask} - 2  : chunkc tiling::monitor -f 2

    # send window to monitor and follow focus
    ctrl - right : chunkc tiling::window --send-to-monitor 1; chunkc tiling::monitor -f 1
    ctrl - left : chunkc tiling::window --send-to-monitor 2; chunkc tiling::monitor -f 2

    # increase region size
    ${modMask} - ${keycodes.LeftBracket} : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge west
    ${modMask} - ${keycodes.RightBracket} : chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge east

    # rotate tree
    ${modMask} - r : chunkc tiling::desktop --rotate 90

    # mirror tree y-axis
    ${modMask} - y : chunkc tiling::desktop --mirror vertical

    # mirror tree x-axis
    ${modMask} - x : chunkc tiling::desktop --mirror horizontal

    # toggle window fullscreen
    ${modMask} - f : chunkc tiling::window --toggle fullscreen

    # toggle window native fullscreen
    shift + ${modMask} - f : chunkc tiling::window --toggle native-fullscreen

    # toggle window parent zoom
    ${modMask} - d : chunkc tiling::window --toggle parent

    # toggle window split type
    ${modMask} - e : chunkc tiling::window --toggle split

    # float / unfloat window and center on screen
    ${modMask} - t : chunkc tiling::window --toggle float \
              chunkc tiling::window --grid-layout 1:1:1:1:1:1

    # toggle sticky, float and resize to picture-in-picture size
    ${modMask} - s : chunkc tiling::window --toggle sticky;\
              chunkc tiling::window --grid-layout 5:5:4:0:1:1
  '';
}
