{ config, lib, pkgs, ... }:

{
  networking.firewall.enable = false; #TODO
  virtualisation.oci-containers.containers = {
    home-assistant = {
      autoStart = true;
      image = "home-assistant/home-assistant:1.0.0b5";
      environment = {
        TZ="Europe/Warsaw";
      };
      ports = [
        "8123:8123"
      ];
      volumes = [
        "/home/peel/wrk/hassio:/config"
        "/etc/localtime:/etc/localtime"
      ];
      extraOptions = [
        "--network=host"
#        "--device=/dev/ttyACM0:/dev/ttyACM0"
      ];
    };
  };
}
