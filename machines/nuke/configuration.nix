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

  environment.systemPackages = with pkgs; [ emacs docker firefox _1passwordAM ];

  users.extraUsers = {
    "${username}"= {
      home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" "libvirtd" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnKhXt4exPC3wNmDw2hBRFHPv6y0ubv0l4RiCzSFl2bxUE6BY1svnsxwkr8kPc2FzAK+cVT4jORb75f4+mSjbJHYq9+IxZIQyqG+4UZQoLgns371PXSlvCbu7G8tvdqYHOs0q07uYNupgtNYVqSrWYUdfNuePDXafY7TqIgMvt8MXN2m9w5dGupoTh5bCpB9yAA/C0dKqCP8gCG/2QK/9ko76bSowvKONwcqlxi4QBXBCWAH5eWkkNX/dpLSrZAEtnS2cfhsThqvp1cJNoM2y2qwWYpu1xsNI+7QmO4p81bpc64giwHbJhE0/N3Am6m4geTYVKsOtMjFMsWtG7Qng2XfUnnb9acoUkVjxejGpZyPHCVQ7VCSQNkui9uD+/lx41PRdLUcCqzgzn9GILTXY9Q8OXMiuE1bVMLyhJFMftz69yB2SG63eVWoGbEbEpSVvS2lnCjTejJVpSpU9a8Abw+DH8isHCHfXFwDJmi/T5RTRrUrlqH5MIkj9UoZiKl6mgRiqMYGNiuTL/hzurKuNraigovBm5zM1NkGYICF9nG4qtxxIApyVNMMlu2kCsSTCgBIFq4xe18tgGUWlsYrWCPVgdkdC9B9vDVzH3VJG3GYVSvuDIFehlwpCWvo31VJTMVIm6JN6CIWiyBN0roETags4h+oDNf2mPptLQFDBzpw== peel@fff666
"
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

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;

    statusPage = true;
        
    # syno
    virtualHosts."px.${orgdomain}" = {
      # enableACME = true;
      # forceSSL = true;
      locations."/" = {
        proxyPass = "https://192.168.1.6:32400";
        proxyWebsockets = true;
      };
    };
    virtualHosts."data.${orgdomain}" = {
      # enableACME = true;
      # forceSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.1.6";
        proxyWebsockets = true;
      };
    };
    virtualHosts."drive.${orgdomain}" = {
      # enableACME = true;
      # forceSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.1.6/drive";
        proxyWebsockets = true;
      };
    };
    virtualHosts."photo.${orgdomain}" = {
      # enableACME = true;
      # forceSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.1.6/photo";
        proxyWebsockets = true;
      };
    };
  };
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
