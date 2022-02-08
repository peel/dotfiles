{ config, pkgs, ... }:

{

  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
  };

  i18n.defaultLocale = "en_US.UTF-8";

  services.dbus = {
    enable = true;
    packages = [ pkgs.dconf ];
  };

  services.openssh = {
    enable = true;
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish.addresses = true;
    publish.enable = true;
    publish.workstation = true;
    publish.domain = true;
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
    liveRestore = true;
  };

  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };
}
