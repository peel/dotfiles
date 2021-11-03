{ config, lib, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    home-assistant = {
      autoStart = true;
      image = "homeassistant/home-assistant:2021.10.0";
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
      image = "esphome/esphome:2021.9.3";
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
    timescaledb = {
      autoStart = true;
      image = "timescale/timescaledb:2.4.2-pg13";
      environment = {
        TZ="Europe/Warsaw";
#	POSTGRES_USER=(builtins.extraBuiltins.pass "hassio-db-user");
#	POSTGRES_PASSWORD=secret "hassio-db-password";
#	POSTGRES_DB=secret "hassio-db-name";
      };
      ports = [
        "5432:5432"
      ];
      volumes = [
        "/home/peel/wrk/hassio/data:/var/lib/postgresql/data"
      ];
    };
  };
}
