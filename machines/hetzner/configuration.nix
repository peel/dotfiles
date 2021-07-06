# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # WARN required for ACME to work
  boot.tmpOnTmpfs = false;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;


  

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
   users.users.peel = {
     isNormalUser = true;
     extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
   };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  #   firefox
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  boot.loader.grub.devices = [ "/dev/sda" ];
  services.openssh.permitRootLogin = "yes";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDG1kHVJm6zcWqDEsACPmy+CgCsrF8yLGpXi0VwHCYGUmvIyjHrmJrr3iAt+4tJprp+FT7CSR/dXk2NajBwxRerncXfFM/nkIdrDPPgO2yxpFIms5dwt8znnjf8T05mW/dXLr7E45uaT4DBqW/eVeZlH3VuJINmi1GJrp+mMKypIFk7DzqM+bQz0MM+qRjH0433XL0Lv+o1aeYfjW12SKWHruMXLDT820T9qefxvhx83hZRGgzTNkof1svBGV9rdCcKzjPsHw1uAWex0KQI3LpuE7fzKwElR2/z2JwoPRiPqum4QilpeyqOGA8xwhJcM/zAHf1hvVuQYlXiSl+QFfMP0Lx5vW/FxZXoa9DJzk5kH0o2ALwE8F/iUOgi2Poy4mq1rhUn26ztSifgWLJLaWBagbn7+dx3dRL1rxBPCJo5cunKBViBpaxdbnSVnaHicoMtQRZ6NUL9g6r1w2oOYftff340LE3kTNzpGhs3vTqsAj0ubwwsFuRK/hjPgv8Rndw8SehuZf+s7kdisBovUyGs/+P15A0pUQ7S65A+N/8zsKZYrnaBxUQ0AqzxPKz2OEuBfwCnFneOwstiUbWs7zNz4BvYIsbCq7mUi7IESzEQ/ttYTG2xHgc38lUlePNazpvxcGWHBcNI1n3SDfhAHpIjWIzXw96AO2JKo+Yl7EpwIQ== cardno:000609697351"
  ];

  networking.wireguard.interfaces = {
     wg0 = {
       ips = [ "192.168.100.1/24" ];
       listenPort = 5553;
       privateKeyFile = "/root/wg-private";
       postSetup = ''
        ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT; 
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o ens3 -j MASQUERADE;
        ${pkgs.iptables}/bin/ip6tables -A FORWARD -i wg0 -j ACCEPT; 
        ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -o ens3 -j MASQUERADE
       '';
       postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o ens3  -j MASQUERADE;
        ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -o wg0 -j MASQUERADE
       '';
       peers = [
         { # nuke 
           publicKey = "g2Zex7dC+OXs/vH0WzeEOO9L6MLDS3MfEO8tkFrM+nI=";
           allowedIPs = [ "192.168.100.2/32" ];
         }];
     };
  };

  networking.nat = {
    enable = true;
    externalInterface = "ens3";
    internalInterfaces = [ "wg0" ];
  };
  
  networking.firewall.enable = true;
  networking.firewall.interfaces.ens3.allowedTCPPorts = [ 22 25 53 80 443 465 587 5000 5001 32400 ];
  networking.firewall.interfaces.ens3.allowedUDPPorts = [ 53 5553 ];
  networking.firewall.interfaces.wg0.allowedTCPPorts = [ 993 ];
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;


  security.acme = {
    email = "plimanowski@pm.me";
    acceptTerms = true;  
  };
  
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    clientMaxBodySize = "2G";
    statusPage = true;

    virtualHosts."px.fff666.org" = {
      default = true;
      enableACME = true;
      addSSL = true;
      http2 = true;
      locations."/" = {
        proxyPass = "http://192.168.100.2";
        proxyWebsockets = true;
        extraConfig = ''
          add_header X-Robots-Tag: noindex;
        '';
      };
    };
    virtualHosts."data.fff666.org" = {
      enableACME = true;
      addSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.100.2/data/";
        proxyWebsockets = true;
        extraConfig = ''
          add_header X-Robots-Tag: noindex;
        '';
      };
    };
    virtualHosts."drive.fff666.org" = {
      enableACME = true;
      addSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.100.2/drive/";
        proxyWebsockets = true;
        extraConfig = ''
          add_header X-Robots-Tag: noindex;
        '';
      };
    };
    virtualHosts."photo.fff666.org" = {
      enableACME = true;
      addSSL = true;
      locations."/" = {
        proxyPass = "http://192.168.100.2/photo/";
        proxyWebsockets = true;
        extraConfig = ''
          add_header X-Robots-Tag: noindex;
        '';
      };
    };
  };

}

