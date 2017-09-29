{ config, pkgs, lib, ... }:

let
  wallpaper = pkgs.copyPathToStore ./art/the-technomancer.png;
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

#  boot.kernelPatches = [
#    { name = "poweroff-fix"; patch = ./patches/kernel/poweroff-fix.patch; }
#    { name = "hid-apple-keyboard"; patch = ./patches/kernel/hid-apple-keyboard.patch; }
#  ];
#  boot.initrd.kernelModules = [
#    "dm_snapshot"
#  ];
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
  nix.useSandbox = true;
  nix.binaryCaches = [ https://cache.nixos.org ];

  networking.hostName = hostName;
  networking.networkmanager.enable = lib.mkForce true;
  networking.wireless.enable = lib.mkForce false;
  networking.firewall.enable = true;

  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
    facetimehd.enable = true;
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    opengl.extraPackages = with pkgs; [ vaapiIntel ];
    pulseaudio.enable = true;
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

  environment.variables.NO_AT_BRIDGE = "1";
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
    graphviz
    jq
    nix-repl
    openjdk
    ranger
    sbt
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
    networkmanagerapplet
    scrot
    xclip xsel
    acpi
    htop
    powertop
    #lm_sensors
    rofi
    dunst
    lightum
    iw
    zeal
    #xorg.xbacklight
    # xlibs.xev
    # xlibs.xkill
    # xorg.xmessage
    # xlibs.xmodmap
    # xlibs.xset
    # xlibs.xwininfo
    # xorg.libXrandr
    # xorg.xbacklight
    # xorg.xf86inputkeyboard
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
    keybase
    keybase-gui

    unclutter-xfixes

  ];


  powerManagement.enable = true;

  programs.light.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;

  # services.openssh.enable = true;
  # services.printing.enable = true;

  services.locate.enable = true;
  services.nixosManual.showManual = true;

  services.xserver = {
    enable = true;
    xkbOptions = "eurosign:e";
    dpi = 168;
    xrandrHeads = [ "eDP1" "DP1" ];
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
      fingersMap = [ 0 0 0];
      buttonsMap = [ 1 3 2 ];
      twoFingerScroll = true;
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
      autoLogin.user = "peel";
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
  users.extraUsers.peel = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/peel";
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
    shadowOpacity = ".5";
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

  services.mopidy = {
    enable = true;
    extensionPackages = [ pkgs.mopidy-local-sqlite pkgs.mopidy-spotify pkgs.mopidy-iris ];
  };

  nixpkgs.config.packageOverrides = pkgs : rec {
    rofi = import ./rofi/rofi.nix { inherit pkgs; terminal = "urxvt"; };
    urxvt = import ./urxvt/urxvt.nix { inherit pkgs; };
    dunst = import ./dunst/dunst.nix { inherit pkgs; browser = "firefox"; };
    stalonetray = import ./stalonetray/stalonetray.nix { inherit pkgs; };
    pragmatapro =
      pkgs.stdenv.mkDerivation rec {
        version = "0.826";
        name = "pragmatapro-${version}";
        src = pkgs.requireFile rec {
          name = "PragmataPro-${version}.zip";
          url = "file://path/to/${name}";
          sha256 = "05r2xkkzgdpcas244yw75g8ifsz3b6ksabsxpa02fx93bz0qf023";
        };
        buildInputs = [ pkgs.unzip ];
        phases = [ "unpackPhase" "installPhase" ];
        sourceRoot = ".";
        installPhase = ''
          install_path=$out/share/fonts/truetype/
          mkdir -p $install_path
          find 'Pragmata Pro Family' -name "*.ttf" -exec cp {} $install_path \;
        '';
      };
  };
}
