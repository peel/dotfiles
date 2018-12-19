{ config, lib, pkgs, ... }:

with lib;

let
  username = "peel";
  hostName = "m1";
  ha = if hostName == "m1" then true else false;
in rec {
  # arm-specific
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.en54able = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "cma=32M"
    "console=tty0"
  ];
  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [ pkgs.raspberrypiWirelessFirmware ];

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";

  system.stateVersion = "18.09";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-18.09-small";
  system.autoUpgrade.dates = "weekly";
  
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
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1hNlicWZhVgkYg6DlSs1CkjbU710RFhIc9uUFdSlk3MLtjidI0Zfbn3bfqyerXwRzOuyL7/bEuqC9lvu7b6KmvEpj1nhuOr0U1ttgIzUvgmz/cwVcVdtLv9Cr21Y2yMRDkAjj3EyLgXXZ+2sii39pYlMHcf7KFCGwgvAlHiI8ThXIW+ZbGqU6NtyoxIBPSZSbZiUz4wz02azipwFGtmGKT37sMeL635YjWqMYMJD453vPdW2F2rpq4B1NZX5L/chZs0FRytjtJJWrm1IEaqUqZXvhktKC6pDLMm/2fuqLgU8DxltXBf5vqimsJ1LPMXGOgQ0OKExiF/Gkv1Ub9x80qcEfpXninz2cvGw0xWpr7FbEShkbalt5MN7kvLIzB4Kmw71L7Af4CxcyJ0cHGGjzg8B92k1gk1rItqM8e8mGeLMrEFPJxMUCJvIE7Ywgbu7kUM8WB5x+Ni4ZmeIUHAtUUad+Z6Xd1Y7lB7190HeWWUR9+yySxOH4pUA3161sCOa5xG/mnW7QtBbD4Tqnc7ouIUq57JpSvOtilTgQAQfS3NZkNbsOv2lHDltPCqk8F4039UCxps6EbJkgNmtV31jqTiDDdQ0fCmOcl8dNygA5cTkWgMjdf1U+va68s92fVsCrwQiKm3zFNGC6VHsSAkEMc29a4d70lWCZlrcWAglZdw== piotr@codearsonist.com"
      ];
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

  imports = [ ./private.nix  ] ++ lib.optionals ha [ ./ha.nix ];
}
