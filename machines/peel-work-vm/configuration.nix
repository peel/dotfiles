{ config, lib, pkgs, ... }:

with lib;

let
  username = "peel";
  hostName = "peel-work-vm";
in {
  imports = [
    ./hardware-configuration.nix
    ../../setup/nixos
  ];

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
  hardware.video.hidpi.enable = true;

  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  system.stateVersion = "21.11";
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = "experimental-features = nix-command flakes";
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
  };

  networking.useDHCP = false;
  networking.interfaces.ens33.useDHCP = true;
  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = [ pkgs.emacs pkgs.sentinelone pkgs.firefox ];

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

  systemd.services.sentinelone = {
     enable = true;
     description = "sentinelone agent";
     wantedBy = [ "multi-user.target" ];
     after = [ "network.target" ];
     environment.S1_AGENT_INSTALL_CONFIG_PATH="/home/sentinelone/config.cfg";
     serviceConfig = {
       User = "sentinelone";
       Group = "sentinelone";
       Type = "oneshot";
       ExecStart = ''
         ${pkgs.sentinelone}/bin/sentinelctl control start
       '';
       ExecStop = ''
         ${pkgs.sentinelone}/bin/sentinelctl control stop
       '';
       Restart = "on-failure";
       RestartSec = 2;
     };
  };

  users.extraUsers.sentinelone = {
    description = "User for sentinelone";
    isNormalUser = true;
    shell = "${pkgs.coreutils}/bin/true";
    home = "/home/sentinelone";
    extraGroups = [ "wheel" ];
  };
  users.groups.sentinelone.members = [
    "sentinelone"
  ];

  services = {
    xserver = {
      enable = true;
      dpi = 220;
      layout = "us";
      xkbOptions = "eurosign:e,caps:ctrl_modifier";
      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
      desktopManager.gnome.enable = false;
      desktopManager.xterm.enable = false;
      displayManager.defaultSession = "none+xmonad";
      displayManager.lightdm = {
        enable = true;
        autoLogin.enable = true;
        autoLogin.user = username;
      };
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
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

  # containers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  virtualisation.vmware.guest.enable = true;
  fileSystems."/mnt" = {
    device = ".host:/";
    fsType = "fuse./run/current-system/sw/bin/vmhgfs-fuse";
    options = ["umask=22" "uid=1000" "gid=1000" "allow_other" "defaults" "auto_unmount"];
  };
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
    liveRestore = true;
  };
}
