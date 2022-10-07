{ config, lib, pkgs, ... }:

let
  dbUser = "hassio";
  dbPassword = "hassio";
  dbName = "hass";
  secrets = import ./secrets.nix;
in {
  systemd.timers.hassio-backup = {
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*-*-* 4:00:00"; # everyday at 4AM
  };
  systemd.services.hassio-backup = {
    description = "Backup hassio directory to nas";
    after = [ "network-pre.target" "docker-home-assistant.service" ];
    wants = [ "network-pre.target" "docker-home-assistant.service" ];
    wantedBy = [ "timers.target" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.WorkingDirectory = "/home/peel/wrk/hassio";
    script = with pkgs; ''
      ${pkgs.gnutar}/bin/tar -cvf hassio-backup.tar /home/peel/wrk/hassio
      ${pkgs.coreutils}/bin/mv hassio-backup.tar /mnt/download
    '';
  };

  virtualisation.oci-containers.containers = {
    home-assistant = {
      autoStart = true;
      image = "homeassistant/home-assistant:2022.9.7";
      environment = {
        TZ="Europe/Warsaw";
      };
      volumes = [
        "/home/peel/wrk/hassio:/config"
        "/etc/localtime:/etc/localtime"
        "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00:/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00"
      ];
      ports = [
        "8123:8123"
      ];
      extraOptions = [
       "--privileged"
       "--network=host"
       "--device=/dev/ttyACM0:/dev/ttyACM0"
      ];
    };
    eufy-security = {
      autoStart = true;
      image = "bropat/eufy-security-ws:0.9.4";
      environment = {
        USERNAME = secrets.eufy.username;
        PASSWORD = secrets.eufy.password;
        COUNTRY = secrets.eufy.country;
      };
      ports = [
        "3001:3000"
      ];
    };
    rtsp-simple-server = {
      autoStart = true;
      image = "aler9/rtsp-simple-server";
      environment = {
        RTSP_PROTOCOLS = "tcp";
      };
      ports = [
        "8554:8554"
        "1935:1935"
      ];
    };
    # clickhouse = {
    #   autoStart = true;
    #   image = "clickhouse/clickhouse-server:22.6";
    #   environment = {
    #     CLICKHOUSE_DB = dbName;
    #     CLICKHOUSE_USER = dbUser;
    #     CLICKHOUSE_PASSWORD = dbPassword;
    #     CLICKHOUSE_DEFAULT_ACCESS_MANAGEMENT = "1";
    #   };
    #   ports = [
    #     "8321:8123" # ui - 8123 already taken by hass
    #     "9000:9000" # client
    #     "9004:9004" # mysql
    #     "9005:9005" # postgres
    #     "9010:9010" # interserver
    #   ];
    #   volumes = [
    #     "/home/peel/wrk/hassio/data:/var/lib/clickhouse"
    #     "/home/peel/wrk/hassio/clickhouse-server/config.d:/var/lib/clickhouse-server/config.d"
    #     "/home/peel/wrk/hassio/clickhouse-server/usert.d:/var/lib/clickhouse-server/usertd.d"
    #     "/home/peel/wrk/hassio/clickhouse-server/initdb.d:/docker-entrypoint-initdb.d"
    #   ];
    # };
  #   esphome = {
  #     autoStart = true;
  #     image = "esphome/esphome:2021.12.0";
  #     environment = {
  #       TZ="Europe/Warsaw";
  #     };
  #     ports = [
  #       "6052:6052"
	#       "6123:6123"
  #     ];
  #     volumes = [
  #       "/home/peel/wrk/esphome:/config"
  #     ];
  #     extraOptions = [
  #      "--privileged"
  #      "--network=host"
  #     ];
  #   };
  };
}
