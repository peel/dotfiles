{ config, lib, pkgs, ... }:

let
  username = "peel";
  xrandr-ext = pkgs.writeShellScriptBin "xrandr-ext" ''
      # cvt 3840 2160 60.00
      # 3840x2160 59.98 Hz (CVT 8.29M9) hsync: 134.18 kHz; pclk: 712.75 MHz
      # Modeline "3840x2160 60.00" 712.75 3840 4160 4576 5312 2160 2163 2168 2237 -hsync +vsync
      xrandr --newmode "3840x2160h60" 712.75 3840 4160 4576 5312 2160 2163 2168 2237 -hsync +vsync
      xrandr --addmode Virtual-1 3840x2160h60
      xrandr -s '3840x2160h60'
    '';
  xrandr-mbp = pkgs.writeShellScriptBin "xrandr-mbp" ''
     # cvt 3456 2234 120
     # 3456x2234 179.94 Hz (CVT) hsync: 287.38 kHz; pclk: 1397.75 MHz
     # Modeline "3456x2234 120.00" 1397.75 3456 3776 4160 4864 2234 2237 2247 2396 -hsync +vsync
      xrandr --newmode "3456x2234h120" 1397.75 3456 3776 4160 4864 2234 2237 2247 2396 -hsync +vsync
      xrandr --addmode Virtual-1 3456x2234h120
      xrandr -s '3456x2234h120'
    '';
in {
  imports = [
    ./hardware-configuration.nix
    ./vmware-guest.nix
  ];

  # Disable the default module and import our override. We have
  # customizations to make this work on aarch64.
  disabledModules = [ "virtualisation/vmware-guest.nix" ];

  # hardware ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  # We require 5.14+ for VMware Fusion on M1.
  boot.kernelPackages = pkgs.linuxPackages_5_15;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.video.hidpi.enable = true;

  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  documentation.nixos.enable = false;
  system.stateVersion = "21.11";

  networking.useDHCP = false;
  networking.interfaces.ens160.useDHCP = true;

  virtualisation.vmware.guest.enable = true;
  environment.systemPackages = [ pkgs.gtkmm3 ] ++ [ xrandr-ext xrandr-mbp ];

  # users ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  users.mutableUsers = false;
  security.sudo.wheelNeedsPassword = false;
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

  # gui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  services.xserver = {
    enable = true;
    dpi = 220;
    layout = "us";
    xkbOptions = "eurosign:e,caps:ctrl_modifier";
    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
    };

    desktopManager = {
      gnome.enable = false;
      xterm.enable = false;
    };

    displayManager = {
      autoLogin.user = username;
      defaultSession = "none+xmonad";
      # AARCH64: For now, on Apple Silicon, we must manually set the
      # display resolution. This is a known issue with VMware Fusion.
      sessionCommands = ''
          ${pkgs.xlibs.xset}/bin/xset r rate 220 40
        '' + (if pkgs.stdenv.system == "aarch64-linux" then ''
          ${pkgs.xorg.xrandr}/bin/xrandr --newmode "3456x2234_120.00" 1397.75 3456 3776 4160 4864 2234 2237 2247 2396 -hsync +vsync
          ${pkgs.xorg.xrandr}/bin/xrandr --addmode Virtual-1 3456x2234_120.00
          ${pkgs.xorg.xrandr}/bin/xrandr -s 3456x2234_120.00
        '' else "");
      lightdm = {
        enable = true;
        autoLogin.enable = true;
      };
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
}
