{ config, pkgs, lib, ... }:

let
  username = "peel";
  hostName = "fff66602";
in
{
  imports = let nur = (import <nurpkgs-peel/modules>); in [
    ./hardware-configuration.nix
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ] ++ [
    nur.battery-notifier
    nur.udiskie
    nur.autocutsel
    nur.dunst
    nur.weechat
  ];

  # mbp config
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # mbp config
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sdb3";
      preLVM = true;
    }
  ];

  # shared config
  boot.cleanTmpDir = true;
  boot.plymouth.enable = true;

  # mbp config
  boot.extraModprobeConfig = ''
    #options libata.force=noncq
    options hid_apple iso_layout=0
    #options resume=/dev/sdb3
    #options snd_hda_intel index=0 model=intel-mac-auto id=PCH
    #options snd_hda_intel index=1 model=intel-mac-auto id=HDMI
    #options snd_hda_intel model=mbp101
    #options hid_apple fnmode=2
  '';

  # shared config
  nixpkgs.config.allowUnfree = true;
  nix.nixPath = [
    "nixpkgs=channel:nixpkgs-unstable"
    "nixos-config=/etc/nixos/configuration.nix"
    "nurpkgs-peel=$HOME/.config/nurpkgs"
    "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays"
    "setup=$HOME/.config/nixpkgs/setup"
  ];
  nixpkgs.overlays = 
    let path = <nixpkgs-overlays> ; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++[ (import <nurpkgs-peel/overlay.nix>) ];
  nix.useSandbox = true;
  nix.binaryCaches = [ https://cache.nixos.org ];

  # shared config
  networking.hostName = hostName;
  networking.networkmanager.enable = true;
  networking.extraHosts = ''
  127.0.0.1  ${hostName}
  '';
  networking.wireless.enable = false;
  networking.firewall.enable = true;

  # mbp config
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

  # mbp config
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

  # shared config
  time.timeZone = "Europe/Warsaw";

  # environment.variables.NO_AT_BRIDGE = "1";

  # shared config
  powerManagement.enable = true;
  programs.light.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  services.acpid.enable = true;

  # shared config
  services.batteryNotifier.enable = true;

  # services.openssh.enable = true;
  # services.printing.enable = true;

  # shared config
  services.locate.enable = true;
  services.nixosManual.showManual = true;

  # mbp config / shared config
  services.xserver = {
    enable = true;
    xkbOptions = "eurosign:e";
    dpi = 168;
#    xrandrHeads = [
#      {
#        output = "eDP1";
#        primary = true;
#        monitorConfig = ''
#          Option "mode" "2560x1600"
#          Option "pos" "3840x0"
#          Option "rotate" "normal"
#        '';
#      }
    #   {
    #     output = "HDMI2";
    #     monitorConfig = ''
    #       Option "mode" "3840x2160"
    #       Option "pos" "0x0"
    #       Option "rotate" "normal"
    #     '';
    #   }
    #];
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
      # accelFactor = "0.001";
      additionalOptions = ''
        Option "VertScrollDelta" "-180" # scroll sensitivity, the bigger the negative number = less sensitive
        Option "HorizScrollDelta" "-180"
      #   Option "FingerLow" "40"
      #   Option "FingerHigh" "70"
        Option "Resolution" "270" # Pointer sensitivity, this is for a retina screen, so you'll probably need to change this for an air
      '';
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
    displayManager = {
      sessionCommands = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge <<< "Xcursor.size: 64"
      '';
      lightdm = {
        enable = true;
        autoLogin.enable = true;
        autoLogin.user = username;
      };
    };
  };


  # shared config
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;
  security.sudo.extraConfig = ''
    ALL ALL = (root) NOPASSWD: ${pkgs.iw}/bin/iw
    ALL ALL = (root) NOPASSWD: ${pkgs.light}/bin/light
    ALL ALL = (root) NOPASSWD: ${pkgs.systemd}/bin/shutdown
    ALL ALL = (root) NOPASSWD: ${pkgs.systemd}/bin/reboot
  '';

  # shared config
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
  programs.ssh = {
    startAgent = true;
    agentTimeout = "1d";
  };
  programs.gnupg.agent = {
    enable = true;
  };


  # shared config
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

  # shared config
  virtualisation.docker.enable = true;

  # shared config
  services.printing = {
    enable = true;
    drivers = [pkgs.splix];
  };

  # shared config
  services.keybase.enable = true;
  services.kbfs = {
    enable = true;
    mountPoint = "/keybase";
  };

  # mbp config
  services.mbpfan.enable = true;
  # shared config
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

  # mbp config
  services.actkbd.enable = true;
  services.actkbd.bindings = [
    { keys = [ 224 ]; events = [ "key" "rep" ]; command = "${pkgs.light}/bin/light -U 4"; }
    { keys = [ 225 ]; events = [ "key" "rep" ]; command = "${pkgs.light}/bin/light -A 4"; }
    { keys = [ 229 ]; events = [ "key" "rep" ]; command = "${pkgs.kbdlight}/bin/kbdlight down"; }
    { keys = [ 230 ]; events = [ "key" "rep" ]; command = "${pkgs.kbdlight}/bin/kbdlight up"; }
  ];

  # shared config
  services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

  # shared config
  services.redshift = {
    enable = true;
    latitude = "54.372158";
    longitude = "18.638306";
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
  };
  services.emacs.enable = true;
  services.dunst.enable = true;
  services.autorandr.enable = true;
  services.autocutsel.enable = true;
  services.udiskie.enable = true;
  services.weechat = {
    enable = true;
    home = "/home/${username}/.weechat";
    portsToOpen = [ 40900 ];
    withSlack = true;
    withMatrix = true;
  };

  # shared config
  nixpkgs.config.packageOverrides = pkgs : rec {
    bluez = pkgs.bluez5;
  };
}
