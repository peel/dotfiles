{ config, pkgs, lib, ... }:

let
  wallpaper = pkgs.copyPathToStore ./art/the-technomancer.png;
  username = "peel";
  hostName = "fff66602";
  gtk2-theme = import ./paper-gtk2-theme.nix pkgs;
in
{
  imports = [
      ./hardware-configuration.nix
      ./setup/common.nix
      ./setup/nixos.nix
      gtk2-theme
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sdb3";
      preLVM = true;
    }
  ];

  boot.cleanTmpDir = true;
  boot.extraModprobeConfig = ''
    #options libata.force=noncq
    options hid_apple iso_layout=0
    #options resume=/dev/sdb3
    #options snd_hda_intel index=0 model=intel-mac-auto id=PCH
    #options snd_hda_intel index=1 model=intel-mac-auto id=HDMI
    #options snd_hda_intel model=mbp101
    #options hid_apple fnmode=2
  '';

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import (builtins.toPath "/home/${username}/.config/nixpkgs/overlays/${username}.nix")) ];
  nix.useSandbox = true;
  nix.binaryCaches = [ https://cache.nixos.org ];

  networking.hostName = hostName;
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
  networking.firewall.enable = true;

  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
    facetimehd.enable = true;
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    opengl.extraPackages = with pkgs; [ vaapiIntel ];
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    pulseaudio.systemWide = false;
    pulseaudio.support32Bit = true;
    pulseaudio.daemon.config = {
      flat-volumes = "no";
    };
    bluetooth.enable = true;
  };
  sound.mediaKeys.enable = true;

  # Enable the backlight on rMBP
  # Disable USB-based wakeup
  # see: https://wiki.archlinux.org/index.php/MacBookPro11,x
  systemd.services.mbp-fixes = {
    description = "Fixes for MacBook Pro";
    wantedBy = [ "multi-user.target" "post-resume.target" ];
    after = [ "multi-user.target" "post-resume.target" ];
    script = ''
      if [[ "$(cat /sys/class/dmi/id/product_name)" == "MacBookPro11,3" ]]; then
        if [[ "$(${pkgs.pciutils}/bin/setpci  -H1 -s 00:01.00 BRIDGE_CONTROL)" != "0000" ]]; then
          ${pkgs.pciutils}/bin/setpci -v -H1 -s 00:01.00 BRIDGE_CONTROL=0
        fi
        echo 5 > /sys/class/leds/smc::kbd_backlight/brightness
        if ${pkgs.gnugrep}/bin/grep -q '\bXHC1\b.*\benabled\b' /proc/acpi/wakeup; then
          echo XHC1 > /proc/acpi/wakeup
        fi
      fi
    '';
    serviceConfig.Type = "oneshot";
  };


  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  time.timeZone = "Europe/Warsaw";

  # environment.variables.NO_AT_BRIDGE = "1";
  environment.systemPackages = with pkgs; [
    gitFull
    gitAndTools.hub
    stow

    autojump
    aspell
    curl
    #elixir
    #elmPackages.elm
    emacs
    #erlang
    fasd
    gist
    gnupg
    gopass
    graphviz
    jq
    nix-repl
    openjdk
    ranger
    sbt
    awscli
    docker
    docker_compose
    #transmission
    weechat

    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.stack
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.yeganesh
    cabal2nix

    feh
    stalonetray
    blueman
    scrot
    xclip xsel
    acpi
    htop
    powertop
    libnotify
    wirelesstools
    #lm_sensors
    rofi
    dunst
    lightum
    iw
    autorandr
    arandr
    xfce.thunar
    xfce.thunar-dropbox-plugin
    xfce.thunar-archive-plugin
    xfce.thunar_volman
    xfce.xfce4_power_manager
    zeal
    bluez
    #xorg.xbacklight
    # xlibs.xev
    # xlibs.xkill
    # xorg.xmessage
    # xlibs.xmodmap
    # xlibs.xset
    # xlibs.xwininfo
    xorg.libXrandr
    xorg.xbacklight
    xorg.xf86inputkeyboard
    # xorg.xmodmap

    # gtk-engine-murrine
    # gtk_engines
    # arc-gtk-theme
    # # numix-gtk-theme
    # # numix-icon-theme
    # # numix-icon-theme-circle
    # # hicolor_icon_theme
    # # gnome.gnomeicontheme
    # elementary-icon-theme

    # lockScreen
    # comptonStart
    # comptonToggle

    urxvt
    #dropbox
    firefox
    spotify
    keybase
    keybase-gui

    unclutter-xfixes

  ];


  powerManagement.enable = true;

  programs.light.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  services.acpid.enable = true;
  systemd.user.timers."lowbatt" = {
    description = "check battery level";
    timerConfig.OnBootSec = "1m";
    timerConfig.OnUnitInactiveSec = "1m";
    timerConfig.Unit = "lowbatt.service";
    wantedBy = ["timers.target"];
  };
  systemd.user.services."lowbatt" = {
    description = "battery level notifier";
    script = ''
      export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/BAT0/capacity)
      export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/BAT0/status)

      export notify_capacity=10
      export shutdown_capacity=5

      if [[ $battery_capacity -le $notify_capacity && $battery_status = "Discharging" ]]; then
          ${pkgs.libnotify}/bin/notify-send --urgency=critical --hint=int:transient:1 --icon=battery_empty "Battery Low" "You should probably plug-in."
      fi

      if [[ $battery_capacity -le $shutdown_capacity && $battery_status = "Discharging" ]]; then
          ${pkgs.libnotify}/bin/notify-send --urgency=critical --hint=int:transient:1 --icon=battery_empty "Battery Critically Low" "Computer will suspend in 60 seconds."
          sleep 60s

          battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/BAT0/status)
          if [[ $battery_status = "Discharging" ]]; then
              systemctl suspend
          fi
      fi
    '';
  };

  # services.openssh.enable = true;
  # services.printing.enable = true;

  services.locate.enable = true;
  services.nixosManual.showManual = true;

  services.xserver = {
    enable = true;
    xkbOptions = "eurosign:e";
    dpi = 168;
    xrandrHeads = [
      {
        output = "eDP1";
        primary = true;
        monitorConfig = ''
          Option "mode" "2560x1600"
          Option "pos" "3840x0"
          Option "rotate" "normal"
        '';
      }
      {
        output = "HDMI2";
        monitorConfig = ''
          Option "mode" "3840x2160"
          Option "pos" "0x0"
          Option "rotate" "normal"
        '';
      }
    ];
    multitouch.enable = true;
    multitouch.invertScroll = true;
    autoRepeatDelay = 200;
    autoRepeatInterval = 33; # 30hz

    deviceSection = ''
      Option   "NoLogo"         "TRUE"
      Option   "DPI"            "168 x 168"
      Option   "Backlight"      "gmux_backlight"
      Option   "RegistryDwords" "EnableBrightnessControl=1"
    '';
    synaptics = {
      enable = true;
      tapButtons = true;
      fingersMap = [ 1 1 1 ];
      buttonsMap = [ 1 3 2 ];
      twoFingerScroll = true;
      scrollDelta = 250;
    };
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    desktopManager = {
      xterm.enable = false;
      default = "none";
    };
    displayManager.lightdm = {
      enable = true;
      background = wallpaper;
      autoLogin.enable = true;
      autoLogin.user = username;
    };
  };


  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    ALL ALL = (root) NOPASSWD: ${pkgs.iw}/bin/iw
    ALL ALL = (root) NOPASSWD: ${pkgs.light}/bin/light
    ALL ALL = (root) NOPASSWD: ${pkgs.systemd}/bin/shutdown
    ALL ALL = (root) NOPASSWD: ${pkgs.systemd}/bin/reboot
  '';
  users.extraUsers."${username}" = {
    isNormalUser = true;
    uid = 1000;
    home = builtins.toPath "/home/${username}";
    extraGroups = [
      "wheel"
      "docker"
      "networkmanager"
      "messagebus"
      "systemd-journal"
      "disk"
      "audio"
      "video"
    ];
    createHome = true;
    shell = pkgs.fish;
  };


  fonts = {
    fontconfig.enable = true;
    fontconfig.dpi = 180;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      pragmatapro
      source-code-pro
      emojione
      font-awesome-ttf
    ];
  };

  virtualisation.docker.enable = true;

  services.keybase.enable = true;
  services.kbfs = {
    enable = true;
    mountPoint = "/keybase";
  };

  services.mbpfan = {
    enable = true;
    lowTemp = 61;
    highTemp = 65;
    maxTemp = 84;
  };

  services.compton = {
    enable = true;
    # refreshRate = 144;
    inactiveOpacity = "0.95";
    menuOpacity = "0.8";
    fade = true;
    fadeDelta = 3;
    shadow = true;
    shadowOffsets = [ (-8) (-8) ];
    # shadowOpacity = ".5";
    fadeSteps = ["0.25" "0.25"];
    vSync = "opengl-swc";
    extraOptions = ''
      unredir-if-possible = true;
      no-dock-shadow = true;
      clear-shadow = true;
      shadow-radius = 10;
      detect-rounded-corners = true;
      shadow-ignore-shaped = true;
      # Window type settings
      wintypes:
      {
        notify = { fade = true; shadow = true; opacity = 0.9; focus = true; };
        tooltip = { fade = true; shadow = true; opacity = 0.9; focus = true; };
      };
    '';
  };

  services.redshift = {
    enable = true;
    latitude = "54.372158";
    longitude = "18.638306";
  };
  services.actkbd.enable = true;
  services.actkbd.bindings = [
    { keys = [ 224 ]; events = [ "key" "rep" ]; command = "${pkgs.light}/bin/light -U 4"; }
    { keys = [ 225 ]; events = [ "key" "rep" ]; command = "${pkgs.light}/bin/light -A 4"; }
    { keys = [ 229 ]; events = [ "key" "rep" ]; command = "${pkgs.kbdlight}/bin/kbdlight down"; }
    { keys = [ 230 ]; events = [ "key" "rep" ]; command = "${pkgs.kbdlight}/bin/kbdlight up"; }
  ];

  services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

  services.avahi = {
    enable = true;
    nssmdns = true;
  };


  services.emacs.enable = true;
  programs.browserpass.enable = true;

  systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
  };

  systemd.user.services."autocutsel" = {
    enable = true;
    description = "AutoCutSel";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };

  systemd.user.services."udiskie" = {
    enable = true;
    description = "udiskie to automount removable media";
    wantedBy = [ "default.target" ];
    path = with pkgs; [
      gnome3.defaultIconTheme
      gnome3.gnome_themes_standard
      pythonPackages.udiskie
    ];
    environment.XDG_DATA_DIRS="${pkgs.gnome3.defaultIconTheme}/share:${pkgs.gnome3.gnome_themes_standard}/share";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.python27Packages.udiskie}/bin/udiskie -a -t -n -F ";
  };

  nixpkgs.config.packageOverrides = pkgs : rec {
    bluez = pkgs.bluez5;
    rofi = import ./rofi/rofi.nix { inherit pkgs; terminal = "urxvt"; };
    urxvt = import ./urxvt/urxvt.nix { inherit pkgs; };
    dunst = import ./dunst/dunst.nix { inherit pkgs; browser = "firefox"; };
    stalonetray = import ./stalonetray/stalonetray.nix { inherit pkgs; };
  };
}
