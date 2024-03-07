# TODO:
#  - build libangle, libepoxy-libangle
#  - build virgl-libepoxy-libangle

#   nix run .#packages.aarch64-linux.demo

{ modulesPath, config, lib, pkgs, ... }:

let
  username = "peel";
  virgl = pkgs.stdenv.mkDerivation rec {
    pname = "virglrenderer";
    version = "2023-04-05";
    src = pkgs.fetchurl {
      url = "https://gitlab.freedesktop.org/virgl/virglrenderer/-/archive/77f600326956f4832a7999c53b2acf10e426f93c/virglrenderer-77f600326956f4832a7999c53b2acf10e426f93c.tar.gz";
      sha256 = "sha256-EJmgDWj3GuIAINxV4qALYgLit31YKFPBOb0dKxLuIC0=";
    };
    buildInputs = [ pkgs.libepoxy ] ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.libGLU pkgs.xorg.libX11 pkgs.mesa ];
    nativeBuildInputs = [ pkgs.cmake pkgs.meson pkgs.ninja pkgs.pkg-config pkgs.python3 ];
    dontUseCmakeConfigure = true;
    mesonFlags = [
      "-D drm=disabled"
    ];
    meta = with lib; {
      description = "A virtual 3D GPU library that allows a qemu guest to use the host GPU for accelerated 3D rendering";
      homepage = "https://virgil3d.github.io/";
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };
in {
  imports = [
    ./hardware-configuration.nix
  ];

  # hardware ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.growPartition = true;
  boot.loader.grub.device = pkgs.lib.mkDefault "/dev/vda";
  # hardware.video.hidpi.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  documentation.nixos.enable = false;
  system.stateVersion = "22.11";

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;

  environment.systemPackages = [ pkgs.gtkmm3 ];

  # users ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  users.mutableUsers = true;
  security.sudo.wheelNeedsPassword = false;

  users.users = {
    "${username}"= {
      home = "/home/${username}";
      password = "${username}";
      uid = 501;
      createHome = true;
      useDefaultShell = true;
      group = "users";
      extraGroups = [ "wheel" "docker" "video" "lp" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnKhXt4exPC3wNmDw2hBRFHPv6y0ubv0l4RiCzSFl2bxUE6BY1svnsxwkr8kPc2FzAK+cVT4jORb75f4+mSjbJHYq9+IxZIQyqG+4UZQoLgns371PXSlvCbu7G8tvdqYHOs0q07uYNupgtNYVqSrWYUdfNuePDXafY7TqIgMvt8MXN2m9w5dGupoTh5bCpB9yAA/C0dKqCP8gCG/2QK/9ko76bSowvKONwcqlxi4QBXBCWAH5eWkkNX/dpLSrZAEtnS2cfhsThqvp1cJNoM2y2qwWYpu1xsNI+7QmO4p81bpc64giwHbJhE0/N3Am6m4geTYVKsOtMjFMsWtG7Qng2XfUnnb9acoUkVjxejGpZyPHCVQ7VCSQNkui9uD+/lx41PRdLUcCqzgzn9GILTXY9Q8OXMiuE1bVMLyhJFMftz69yB2SG63eVWoGbEbEpSVvS2lnCjTejJVpSpU9a8Abw+DH8isHCHfXFwDJmi/T5RTRrUrlqH5MIkj9UoZiKl6mgRiqMYGNiuTL/hzurKuNraigovBm5zM1NkGYICF9nG4qtxxIApyVNMMlu2kCsSTCgBIFq4xe18tgGUWlsYrWCPVgdkdC9B9vDVzH3VJG3GYVSvuDIFehlwpCWvo31VJTMVIm6JN6CIWiyBN0roETags4h+oDNf2mPptLQFDBzpw== peel@fff666"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC/NSWTPt4wYBay+78bclqOjD5vtaKmiu1zRmzrhsoChbfVYeuYM8TS+pGbiUqMXhRwIfFBqjPAMvEqlwhGuOTe42c0GKjGBq2kvF6p6/AiyutsRcMsKYqNj+3sR6Xx4J7awYvL6OjrzCgOoXonp2jzoMGbAxm6sHJ0Kg+1kLnlgJHGIveshzDUvnYhDd2xH3Y7jGVqzpekqaPEGcauCi91l0lKgjo2hiRWDM/Ho1gHgq6jpDsZIUAXbdPtQh7GTx7luUYUTc0SX08+YmlF8kQ2mmcLQV/Uy5tQMOZ0sqv1RLcraZd87nbJlYqXm97VODwHWNzAbm49kqwGXT2j+CnF0PrSLRABkcgkyaiJWx779vQKRzceENVk9WipavO07poL23egfiB5cYtU+jsC3F6uv61je0E6Z7/w8txb082gdwVVFxrPfoV4qDLgdISZabHV4/Ivp9RWiCtgauHjpv8qhg48IouQqLQT90uuoeA1TLU1ckNDHbqhvDtzmArZWJU= peel@snowberry"
      ];
    };
  };

  # create directories for home-manager
  # fails otherwise :o
  systemd.services.home-manager-directories = {
    description = "Backup hassio directory to nas";
    wantedBy = [ "multi-user.target" ];
    before = [ "home-manager-${username}.service" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.SyslogIdentifier = "hm-directories-${username}";
    serviceConfig.RemainAfterExit = "yes";
    serviceConfig.TimeoutStartSec = 30;
    script = with pkgs; ''
      ${pkgs.coreutils}/bin/install -d -m 0755 -o ${username} -g wheel /nix/var/nix/{profiles,gcroots}/per-user/${username}/
    '';
  };

  services.spice-vdagentd.enable = true;
  virtualisation.qemu.guestAgent.enable = true;
  virtualisation.qemu.package = (config.virtualisation.host.pkgs.qemu.override {
    virglrenderer = virgl;
    virglSupport = true;
    hostCpuOnly = true;
  });
  #   .overrideAttrs(_: old: {
  #   buildInputs = old.buildInputs ++ [ config.virtualisation.host.pkgs.angle ];
  # });
  virtualisation.qemu.options = [
    "-device intel-hda"
    "-device hda-output"
    "-device qemu-xhci"
    "-device virtio-gpu-gl"
    "-display cocoa,gl=es"
    "-device virtio-serial-pci"
    "-chardev qemu-vdagent,id=spice,name=vdagent,clipboard=on"
    "-device virtserialport,chardev=spice,name=com.redhat.spice.0"
  ];
  virtualisation.cores = 8;
  virtualisation.memorySize = 12288;
  virtualisation.diskSize = 30*1024;
  virtualisation.resolution = let notch = 38; in {
    x = 3456-notch;
    y = 2234;
  };
  virtualisation.rosetta.enable = true;
  virtualisation.writableStore = true;
  virtualisation.writableStoreUseTmpfs = false;

  # gui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  security.polkit.enable = true;
  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        command = "${pkgs.sway}/bin/sway";
        user = username;
      };
      default_session = initial_session;
    };
  };
}
