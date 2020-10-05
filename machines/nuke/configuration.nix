{ config, lib, pkgs, ... }:

with lib;

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
  username = "peel";
  hostName = "nuke";
  domain = builtins.extraBuiltins.pass "duckdns.domain";
  orgdomain = builtins.extraBuiltins.pass "organisation.domain";
in {
  imports = let nur = (import <nurpkgs-peel/modules>); in [
    ./hardware-configuration.nix
    <dotfiles/setup/nixos>
    <dotfiles/setup/common/homebridge.nix>
  ] ++ [
    nur.udiskie
  ];

  peel.gui.enable = false;
  
  nixpkgs.config.allowBroken = true;
  nix.nixPath = [
    "nixpkgs=${sources.nixpkgs}"
    "nixos-config=$HOME/.config/nixpkgs/machines/${hostName}/configuration.nix"
    "nurpkgs-peel=${sources.nurpkgs}"
    "dotfiles=$HOME/.config/nixpkgs"
  ];
  nixpkgs.overlays = 
    let path = <dotfiles/overlays> ; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++[ (import <nurpkgs-peel/overlay.nix>) ];
    
  # hardware ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  hardware.enableAllFirmware = true;
  boot.kernelModules = [ "iwlwifi" ]; 
  boot.loader = {
    efi.efiSysMountPoint = "/efi";
    grub = {
      device = "nodev"; # do not install Grub for BIOS booting
      efiSupport = true; 
      extraInitrd = "/boot/initrd.keys.gz";  # add LUKS key to the initrd
      enableCryptodisk = true; # allow Grub to boot from LUKS devices
      zfsSupport = true;
    };
    grub.efiInstallAsRemovable = true;
  };
  boot.initrd.luks.devices.nvme = {
   device = "/dev/disk/by-uuid/47543190-aed6-44d1-bc69-ef1f65df0bbd";
   keyFile = "/keyfile.bin";
  };
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  system.stateVersion = "18.03";
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
  };

  networking = {
    hostId = "675e1435";
    hostName = hostName;
    wireless.enable = false;
    networkmanager.enable = true;
    firewall.enable = false;
  };

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = [ pkgs.emacs pkgs.docker ];

  users.extraUsers = {
    "${username}"= {
      home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnKhXt4exPC3wNmDw2hBRFHPv6y0ubv0l4RiCzSFl2bxUE6BY1svnsxwkr8kPc2FzAK+cVT4jORb75f4+mSjbJHYq9+IxZIQyqG+4UZQoLgns371PXSlvCbu7G8tvdqYHOs0q07uYNupgtNYVqSrWYUdfNuePDXafY7TqIgMvt8MXN2m9w5dGupoTh5bCpB9yAA/C0dKqCP8gCG/2QK/9ko76bSowvKONwcqlxi4QBXBCWAH5eWkkNX/dpLSrZAEtnS2cfhsThqvp1cJNoM2y2qwWYpu1xsNI+7QmO4p81bpc64giwHbJhE0/N3Am6m4geTYVKsOtMjFMsWtG7Qng2XfUnnb9acoUkVjxejGpZyPHCVQ7VCSQNkui9uD+/lx41PRdLUcCqzgzn9GILTXY9Q8OXMiuE1bVMLyhJFMftz69yB2SG63eVWoGbEbEpSVvS2lnCjTejJVpSpU9a8Abw+DH8isHCHfXFwDJmi/T5RTRrUrlqH5MIkj9UoZiKl6mgRiqMYGNiuTL/hzurKuNraigovBm5zM1NkGYICF9nG4qtxxIApyVNMMlu2kCsSTCgBIFq4xe18tgGUWlsYrWCPVgdkdC9B9vDVzH3VJG3GYVSvuDIFehlwpCWvo31VJTMVIm6JN6CIWiyBN0roETags4h+oDNf2mPptLQFDBzpw== peel@fff666
"
      ];
    };
  };

  services = {
    openssh = {
      enable = true;
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


  # monitoring  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  users.extraUsers.datadog.extraGroups = [ "docker" "systemd-journal" ];
  services.datadog-agent = {
    enable = true;
    extraConfig = { logs_enabled = true; };
    logLevel = "DEBUG";
    extraIntegrations = {
      snmp = (ps: with ps; [ pyasn1 pysnmp pycryptodomex pysmi ply ]);
    };
    checks = {
      journald = {
        logs = [ { type = "journald"; include_units = [ "docker.service" "nginx.service" ]; } ];
      };
      nginx = {
        init_config = null;
        instances = [ { nginx_status_url = "http://localhost:80/nginx_status"; } ];
      };
    };
    apiKeyFile = builtins.extraBuiltins.pass "datadog.key";
  };

  # general routes  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  services.fail2ban.enable = true;
  services.dnsmasq =
    let nuke = builtins.extraBuiltins.pass "nuke.ip";
    in {
      enable = true;
      servers = [ "1.1.1.1" "1.0.0.1" ];
      extraConfig = ''
        address=/.${orgdomain}/${nuke}
      '';
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    statusPage = true;
    
    # syno
    virtualHosts."drive.${orgdomain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://datavism.local:5000";
        proxyWebsockets = true;
      };
    };

    # hass
    virtualHosts."ha.${orgdomain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://localhost:8123";
        proxyWebsockets = true;
      };
    };
  };
  
  # containers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
    liveRestore = true;
  };
  
  # enable access to external network from containers
  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["ve-+"];
  networking.nat.externalInterface = "eth0";
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];
  
  containers = {
    # hass = {
    #   config = import <setup/ha.nix>;
    #   autoStart = true;
    # };
    # plex = {
    #   config = import <setup/plex.nix>;
    #   autoStart = true;
    # };
    # vault = {};
  };
  
}
