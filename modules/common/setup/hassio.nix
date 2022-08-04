{ config, lib, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    home-assistant = {
      autoStart = true;
      image = "homeassistant/home-assistant:2022.8.0";
      environment = {
        TZ="Europe/Warsaw";
      };
      ports = [
        "8123:8123"
      ];
      volumes = [
        "/home/peel/wrk/hassio:/config"
        "/etc/localtime:/etc/localtime"
        "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00:/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00"
      ];
      extraOptions = [
       "--privileged"
       "--network=host"
       "--device=/dev/ttyACM0:/dev/ttyACM0"
      ];
    };
    esphome = {
      autoStart = true;
      image = "esphome/esphome:2021.12.0";
      environment = {
        TZ="Europe/Warsaw";
      };
      ports = [
        "6052:6052"
	"6123:6123"
      ];
      volumes = [
        "/home/peel/wrk/esphome:/config"
      ];
      extraOptions = [
       "--privileged"
       "--network=host"
      ];
    };
  };
}
