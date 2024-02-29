{ config, lib, pkgs, ... }:

with lib;

let
  username = "peel";
  hostName = "nuke";
  # domain = builtins.extraBuiltins.pass "duckdns.domain";
  orgdomain = "fff666.org"; # builtins.extraBuiltins.pass "organisation.domain";
  s = import ../s.nix;
in {
  imports = [
    ./hardware-configuration.nix
  ];

  #peel.gui.enable = false;

  nixpkgs.config.allowBroken = true;

  # hardware ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  #boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  hardware.bluetooth.package = pkgs.bluez;
  hardware.bluetooth.disabledPlugins = ["sap"];
  # hardware.bluetooth.settings = {
  #   General.ControllerMode = "dual";
  # };
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

  users.extraUsers = {
    "${username}"= {
      home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnKhXt4exPC3wNmDw2hBRFHPv6y0ubv0l4RiCzSFl2bxUE6BY1svnsxwkr8kPc2FzAK+cVT4jORb75f4+mSjbJHYq9+IxZIQyqG+4UZQoLgns371PXSlvCbu7G8tvdqYHOs0q07uYNupgtNYVqSrWYUdfNuePDXafY7TqIgMvt8MXN2m9w5dGupoTh5bCpB9yAA/C0dKqCP8gCG/2QK/9ko76bSowvKONwcqlxi4QBXBCWAH5eWkkNX/dpLSrZAEtnS2cfhsThqvp1cJNoM2y2qwWYpu1xsNI+7QmO4p81bpc64giwHbJhE0/N3Am6m4geTYVKsOtMjFMsWtG7Qng2XfUnnb9acoUkVjxejGpZyPHCVQ7VCSQNkui9uD+/lx41PRdLUcCqzgzn9GILTXY9Q8OXMiuE1bVMLyhJFMftz69yB2SG63eVWoGbEbEpSVvS2lnCjTejJVpSpU9a8Abw+DH8isHCHfXFwDJmi/T5RTRrUrlqH5MIkj9UoZiKl6mgRiqMYGNiuTL/hzurKuNraigovBm5zM1NkGYICF9nG4qtxxIApyVNMMlu2kCsSTCgBIFq4xe18tgGUWlsYrWCPVgdkdC9B9vDVzH3VJG3GYVSvuDIFehlwpCWvo31VJTMVIm6JN6CIWiyBN0roETags4h+oDNf2mPptLQFDBzpw== peel@fff666"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC/NSWTPt4wYBay+78bclqOjD5vtaKmiu1zRmzrhsoChbfVYeuYM8TS+pGbiUqMXhRwIfFBqjPAMvEqlwhGuOTe42c0GKjGBq2kvF6p6/AiyutsRcMsKYqNj+3sR6Xx4J7awYvL6OjrzCgOoXonp2jzoMGbAxm6sHJ0Kg+1kLnlgJHGIveshzDUvnYhDd2xH3Y7jGVqzpekqaPEGcauCi91l0lKgjo2hiRWDM/Ho1gHgq6jpDsZIUAXbdPtQh7GTx7luUYUTc0SX08+YmlF8kQ2mmcLQV/Uy5tQMOZ0sqv1RLcraZd87nbJlYqXm97VODwHWNzAbm49kqwGXT2j+CnF0PrSLRABkcgkyaiJWx779vQKRzceENVk9WipavO07poL23egfiB5cYtU+jsC3F6uv61je0E6Z7/w8txb082gdwVVFxrPfoV4qDLgdISZabHV4/Ivp9RWiCtgauHjpv8qhg48IouQqLQT90uuoeA1TLU1ckNDHbqhvDtzmArZWJU= peel@snowberry"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDG1kHVJm6zcWqDEsACPmy+CgCsrF8yLGpXi0VwHCYGUmvIyjHrmJrr3iAt+4tJprp+FT7CSR/dXk2NajBwxRerncXfFM/nkIdrDPPgO2yxpFIms5dwt8znnjf8T05mW/dXLr7E45uaT4DBqW/eVeZlH3VuJINmi1GJrp+mMKypIFk7DzqM+bQz0MM+qRjH0433XL0Lv+o1aeYfjW12SKWHruMXLDT820T9qefxvhx83hZRGgzTNkof1svBGV9rdCcKzjPsHw1uAWex0KQI3LpuE7fzKwElR2/z2JwoPRiPqum4QilpeyqOGA8xwhJcM/zAHf1hvVuQYlXiSl+QFfMP0Lx5vW/FxZXoa9DJzk5kH0o2ALwE8F/iUOgi2Poy4mq1rhUn26ztSifgWLJLaWBagbn7+dx3dRL1rxBPCJo5cunKBViBpaxdbnSVnaHicoMtQRZ6NUL9g6r1w2oOYftff340LE3kTNzpGhs3vTqsAj0ubwwsFuRK/hjPgv8Rndw8SehuZf+s7kdisBovUyGs/+P15A0pUQ7S65A+N/8zsKZYrnaBxUQ0AqzxPKz2OEuBfwCnFneOwstiUbWs7zNz4BvYIsbCq7mUi7IESzEQ/ttYTG2xHgc38lUlePNazpvxcGWHBcNI1n3SDfhAHpIjWIzXw96AO2JKo+Yl7EpwIQ== cardno:000610595904"
      ];
    };
  };

  services.xserver.enable = true;
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
    };
  };
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.defaultSession = "sway";
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  programs.xwayland.enable = true;
  programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = with pkgs; [
        swaylock
        swayidle
        wl-clipboard
        mako # notification daemon
        alacritty # Alacritty is the default terminal in the config
        dmenu
      ];
      extraSessionCommands = ''
        export WLR_BACKENDS=headless sway
      '';
  };

  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  services.avahi = {
    enable = true;
    nssmdns = true;
    publish.addresses = true;
    publish.enable = true;
    publish.workstation = true;
    publish.domain = true;
  };

  services.apcupsd = {
    enable = true;
    configText = ''
      UPSTYPE usb
      BATTERYLEVEL 20
      NETSERVER on
      NISIP 0.0.0.0
    '';
    hooks = {
      doshutdown = let
        shutdown = pkgs.writeShellScriptBin "ups-shutdown" ''
          echo 'Staring Shutdown Scirpt, initiated by NUT client'
          ssh -i /home/peel/.ssh/id_rsa root@192.168.50.1 'ubnt-systool' & sleep 2 && /sbin/shutdown -h +0
        ''; in
        "${shutdown} >> /var/log/ups/ups.log";
    };
  };

  fileSystems."/mnt/music" = {
    device = "192.168.1.60:/volume1/music";
    fsType = "nfs";
    options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/video" = {
    device = "192.168.1.60:/volume1/video";
    fsType = "nfs";
    options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/download" = {
   device = "192.168.1.60:/volume1/download";
   fsType = "nfs";
   options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/books" = {
   device = "192.168.1.60:/volume1/books";
   fsType = "nfs";
   options = [ "nfsvers=4.1" ];
  };
  fileSystems."/mnt/audiobooks" = {
   device = "192.168.1.60:/volume1/audiobooks";
   fsType = "nfs";
   options = [ "nfsvers=4.1" ];
  };

  # general routes  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  services.fail2ban.enable = true;

  # networking  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  networking.firewall = {
    enable = true;
    trustedInterfaces = [ "tailscale0" ];
    allowedTCPPorts =
      let
        navidrome = [ 4533 4534 3000 ];
        hass = [ 8123 8124 ];
        natsHttp = [ 8222 ];
        esphome = [ 6052 ];
        go2rtc = [ 1984 8555 ];
      in [ 22 53 80 443 5001 8080 8083 21063 21064 21065 21066 32400 1883 ] ++ hass ++ navidrome ++ natsHttp ++ esphome ++ go2rtc;
    allowedUDPPorts = 
      let govee = [ 4001 4002];
      in [ 53 5353 config.services.tailscale.port ] ++ govee;
    checkReversePath = "loose";
  };

  services.tailscale.enable = true;
  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";
    after = [ "network-pre.target" "tailscale.service" ];
    wants = [ "network-pre.target" "tailscale.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = with pkgs; ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${pkgs.tailscale}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${pkgs.tailscale}/bin/tailscale up -authkey ${s.tsnuke}
    '';
  };

  security.acme.acceptTerms = true;
  # security.acme.certs."px.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."e.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."h.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."b.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."k.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."m.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  # security.acme.certs."d.fff666.org" = {
  #   group = "nginx";
  #   email = "digest_yowl.0o@icloud.com";
  #   dnsResolver = "1.1.1.1:53";
  #   dnsProvider = "route53";
  #   credentialsFile = ./r53.conf;
  # };
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    clientMaxBodySize = "2G";
    statusPage = true;

    virtualHosts = {
      # "px.${orgdomain}" = {
      #   useACMEHost = "px.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:32400";
      #     proxyWebsockets = true;
      #   };
      # };
      # "b.${orgdomain}" = {
      #   useACMEHost = "b.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:8083";
      #     proxyWebsockets = true;
      #   };
      # };
      # "e.${orgdomain}" = {
      #   useACMEHost = "e.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:6052";
      #     proxyWebsockets = true;
      #   };
      # };
      # "k.${orgdomain}" = {
      #   useACMEHost = "k.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:65535";
      #     proxyWebsockets = true;
      #   };
      # };
      # "h.${orgdomain}" = {
      #   useACMEHost = "h.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:8123";
      #     proxyWebsockets = true;
      #   };
      # };
      # "m.${orgdomain}" = {
      #   useACMEHost = "m.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "http://127.0.0.1:4533";
      #     proxyWebsockets = true;
      #   };
      # };
      # "d.${orgdomain}" = {
      #   useACMEHost = "d.fff666.org";
      #   http2 = false;
      #   forceSSL = true;
      #   locations."/" = {
      #     proxyPass = "https://192.168.1.60:5001";
      #     proxyWebsockets = true;
      #   };
      # };
    };
  };

  # services.calibre-server = {
  #   enable = true;
  #   libraries = [ "/mnt/books/calibre" ];
  #   user = "root";
  # };
  # services.calibre-web = {
  #   enable = true;
  #   listen.ip = "0.0.0.0";
  #   openFirewall = true;
  #   options = {
  #     calibreLibrary = "/mnt/books/calibre";
  #     enableBookConversion = true;
  #     enableBookUploading = true;
  #   };
  #   user = "root";
  # };

  services.adguardhome = {
    enable = false;
    openFirewall = true;
    settings = {
      # set to the right version here: https://github.com/NixOS/nixpkgs/blob/nixos-22.11/pkgs/servers/adguardhome/default.nix#L19
      schema_version = 14;
      bind_port = 3000;
      bind_host = "0.0.0.0";
      dns = {
        bind_port = 3000;
        bind_host = "0.0.0.0";
        bootstrap_dns = [
          "1.1.1.1"
        ];
      };
    };
  };

}
