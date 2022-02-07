{ config, lib, pkgs, ... }:

with lib;

let
  username = "peel";
  hostName = "nuke";
  # domain = builtins.extraBuiltins.pass "duckdns.domain";
  orgdomain = "fff666.org"; # builtins.extraBuiltins.pass "organisation.domain";
in {
  imports = [
    ./hardware-configuration.nix
    ../../setup/nixos
    ../../setup/common/hassio.nix
  ];

  #peel.gui.enable = false;
  
  nixpkgs.config.allowBroken = true;
  nixpkgs.overlays = 
    let path = ../../overlays ; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)));
    
  # hardware ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  hardware.bluetooth.package = pkgs.bluezFull;
  services.xserver.libinput = {
    enable = true;
    tapping = true;
    middleEmulation = true;
    accelProfile = "adaptive";
    clickMethod = "clickfinger";
    naturalScrolling = true;
    tappingDragLock = true;
  };

  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };

  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  system.stateVersion = "20.09";
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  ''; 
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
  };

  networking = {
    hostId = "675e1435";
    hostName = hostName;
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [  ];

  users.extraUsers = {
    "${username}"= {
      home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" "libvirtd" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnKhXt4exPC3wNmDw2hBRFHPv6y0ubv0l4RiCzSFl2bxUE6BY1svnsxwkr8kPc2FzAK+cVT4jORb75f4+mSjbJHYq9+IxZIQyqG+4UZQoLgns371PXSlvCbu7G8tvdqYHOs0q07uYNupgtNYVqSrWYUdfNuePDXafY7TqIgMvt8MXN2m9w5dGupoTh5bCpB9yAA/C0dKqCP8gCG/2QK/9ko76bSowvKONwcqlxi4QBXBCWAH5eWkkNX/dpLSrZAEtnS2cfhsThqvp1cJNoM2y2qwWYpu1xsNI+7QmO4p81bpc64giwHbJhE0/N3Am6m4geTYVKsOtMjFMsWtG7Qng2XfUnnb9acoUkVjxejGpZyPHCVQ7VCSQNkui9uD+/lx41PRdLUcCqzgzn9GILTXY9Q8OXMiuE1bVMLyhJFMftz69yB2SG63eVWoGbEbEpSVvS2lnCjTejJVpSpU9a8Abw+DH8isHCHfXFwDJmi/T5RTRrUrlqH5MIkj9UoZiKl6mgRiqMYGNiuTL/hzurKuNraigovBm5zM1NkGYICF9nG4qtxxIApyVNMMlu2kCsSTCgBIFq4xe18tgGUWlsYrWCPVgdkdC9B9vDVzH3VJG3GYVSvuDIFehlwpCWvo31VJTMVIm6JN6CIWiyBN0roETags4h+oDNf2mPptLQFDBzpw== peel@fff666
"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDG1kHVJm6zcWqDEsACPmy+CgCsrF8yLGpXi0VwHCYGUmvIyjHrmJrr3iAt+4tJprp+FT7CSR/dXk2NajBwxRerncXfFM/nkIdrDPPgO2yxpFIms5dwt8znnjf8T05mW/dXLr7E45uaT4DBqW/eVeZlH3VuJINmi1GJrp+mMKypIFk7DzqM+bQz0MM+qRjH0433XL0Lv+o1aeYfjW12SKWHruMXLDT820T9qefxvhx83hZRGgzTNkof1svBGV9rdCcKzjPsHw1uAWex0KQI3LpuE7fzKwElR2/z2JwoPRiPqum4QilpeyqOGA8xwhJcM/zAHf1hvVuQYlXiSl+QFfMP0Lx5vW/FxZXoa9DJzk5kH0o2ALwE8F/iUOgi2Poy4mq1rhUn26ztSifgWLJLaWBagbn7+dx3dRL1rxBPCJo5cunKBViBpaxdbnSVnaHicoMtQRZ6NUL9g6r1w2oOYftff340LE3kTNzpGhs3vTqsAj0ubwwsFuRK/hjPgv8Rndw8SehuZf+s7kdisBovUyGs/+P15A0pUQ7S65A+N/8zsKZYrnaBxUQ0AqzxPKz2OEuBfwCnFneOwstiUbWs7zNz4BvYIsbCq7mUi7IESzEQ/ttYTG2xHgc38lUlePNazpvxcGWHBcNI1n3SDfhAHpIjWIzXw96AO2JKo+Yl7EpwIQ== cardno:000610595904"
      ];
    };
  };
  
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
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
  };

  fileSystems."/mnt/music" = {
    device = "192.168.1.6:/volume1/music";
    fsType = "nfs";
    options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/video" = {
    device = "192.168.1.6:/volume1/video";
    fsType = "nfs";
    options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/download" = {
    device = "192.168.1.6:/volume1/download";
    fsType = "nfs";
    options = [ "nfsvers=4.1" ];
  };

  # monitoring  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

  # general routes  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  services.fail2ban.enable = true;

  
  # containers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
    liveRestore = true;
  };
  
  networking.firewall.enable = false; #TODO
  virtualisation.libvirtd.enable = true;
  virtualisation.libvirtd.allowedBridges = ["br0" "virbr0" "tap0"];
  programs.dconf.enable = true;
  users.extraGroups.vboxusers.members = [ username ];
  virtualisation.virtualbox = {
    host.enable = true;
    host.enableExtensionPack = true;
    host.enableHardening = false;
  };
  
  # enable access to external network from containers
  # networking.nat.enable = true;
  # networing.nat.internalInterfaces = ["ve-+"];
  # networking.nat.externalInterface = "wlp0s20f3";
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];
  
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "192.168.100.2/24" ];
      listenPort = 5553;
      privateKeyFile = "/home/peel/wg-private";
      peers = [
        {
          publicKey = "fPBjHcK+P3Xb0OU6f9ITONSoMLZK1l1ixkWz+K4Y6yo=";
          allowedIPs = [ "192.168.100.0/22" ];
          endpoint = "162.55.214.59:5553";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  #services.adguardhome = {
  #  enable = true;
  #  openFirewall = true;
  #};
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    clientMaxBodySize = "2G";
    statusPage = true;
        
    virtualHosts."px.${orgdomain}" = {
      # enableACME = true;
      default = true;
      http2 = false;
      # forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:32400";
        proxyWebsockets = true;
      };
    };
  };
  
}
