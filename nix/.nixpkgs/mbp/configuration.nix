# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
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

  nixpkgs.config.allowUnfree = true; 
  nix.useSandbox = true;
  nix.binaryCaches = [ https://cache.nixos.org ];

  networking.hostName = "peel-nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
  networking.firewall.enable = true;

  hardware.bluetooth.enable = true;
  hardware.facetimehd.enable = true;
  hardware.pulseaudio.enable = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  time.timeZone = "Europe/Warsaw";

  environment.systemPackages = with pkgs; [
    gitFull
    stow
    firefox
    keybase
    keybase-gui
  ];

  powerManagement.enable = true;

  programs.light.enable = true;

  # services.openssh.enable = true;
  # services.printing.enable = true;

  services.locate.enable = true;
  services.nixosManual.showManual = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkbOptions = "eurosign:e";
    multitouch.enable = true;
    multitouch.invertScroll = true;
    synaptics = {
      enable = true;
      tapButtons = true;
      fingersMap = [ 0 0 0];
      buttonsMap = [ 1 3 2 ];
      twoFingerScroll = true;
    };
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };


  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  virtualisation.docker.enable = true;


  services.keybase.enable = true;
  services.kbfs = {   
    enable = true;
    mountPoint = "/keybase";
  };

  services.redshift = {
    enable = true;
    latitude = "54.372158";
    longitude = "18.638306";
  };
}
