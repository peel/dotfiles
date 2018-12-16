{ config, lib, pkgs, ... }:

with lib;

let
  username = "peel";
  hostName = "m1";
in rec {
  # arm-specific
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = ["cma=32M"];
  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [ pkgs.raspberrypiWirelessFirmware ];

  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [ git vim ];
  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

  networking.hostName = hostName;
  networking.wireless.enable = true;
  
  users.extraUsers = {
    "${username}"= {
    home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      uid = 1000;
    };
  };
  
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
    avahi = {
      enable = true;
      nssmdns = true;
      publish.addresses = true;
      publish.enable = true;
      publish.workstation = true;
      publish.domain = true;
    };
    xserver.enable = false;
    nixosManual.enable = false;
  };

  imports = [ ./private.nix  ];
}
