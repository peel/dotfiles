{ config, lib, pkgs, ... }:

let
  secrets = import ./secrets.nix;
  cfg = config.peel.hassio;
in {
  options.peel.hassio = {
    enable = lib.mkEnableOption "hassio";
    zigbee2mqtt = lib.mkOption {
      default = pkgs.zigbee2mqtt;
    };
    home-assistant = lib.mkOption {
      default = "2023.11.2";
    };
  };
  config = {
    services.navidrome = {
      enable = true;
      settings = {
        MusicFolder = "/mnt/music";
        ScanSchedule = "@every 20m";
        Address = "0.0.0.0";
        Port = 4533;
      };
    };
    services.jellyfin = {
      enable = true;
      user = "root";
      group = "root";
      openFirewall = true;
    };
    services.zigbee2mqtt = {
      enable = true;
      package = cfg.zigbee2mqtt;
      settings = {
        homeassistant = true;
        frontend = {
          port = 8124;
        };
        permit_join = false;
        mqtt = {
          base_topic = "homeassistant/z2m";
          server = "mqtt://127.0.0.1";
          user = secrets.mqtt.username;
          password = secrets.mqtt.password;
        };
        serial = {
          port = "/dev/ttyACM0";
        };
      };
    };
    services.nats = {
      enable = true;
      jetstream = true;
      settings = {
        mqtt = {
          port = 1883;
          authorization = {
            username = secrets.mqtt.username;
            password = secrets.mqtt.password;
          };
        };
      };
    };
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
      # expose navidrome to sonos
      bonob = {
        autoStart = true;
        image = "simojenki/bonob";
        environment = {
          BNB_PORT = "4534";
          BNB_SONOS_SERVICE_NAME = "nuke";
          BNB_SONOS_SEED_HOST = "192.168.1.76";
          BNB_SONOS_AUTO_REGISTER = "true";
          BNB_SONOS_DEVICE_DISCOVERY = "true";
        };
        ports = [
          "4534:4534"
        ];
        extraOptions = [
          "--network=host"
        ];
      };
      home-assistant = {
        autoStart = true;
        image = "homeassistant/home-assistant:${cfg.home-assistant}";
        environment = {
          TZ="Europe/Warsaw";
        };
        volumes = [
          "/home/peel/wrk/hassio:/config"
          "/etc/localtime:/etc/localtime"
          "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00:/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00" # zha
          "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2686951-if00:/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2686951-if00" # zigbee2mqtt
 	        "/run/dbus:/run/dbus:ro"
        ];
        ports = [
          "8123:8123"
        ];
        extraOptions = [
          "--privileged"
          "--network=host"
          "--device=/dev/ttyACM0:/dev/ttyACM0"
          "--device=/dev/ttyACM1:/dev/ttyACM1"
        ];
      };
      eufy-security = {
        autoStart = true;
        image = "bropat/eufy-security-ws:latest";
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
      influxdb = {
        autoStart = true;
        image = "influxdb:2.4-alpine";
        environment = {
          DOCKER_INFLUXDB_INIT_USERNAME=secrets.influxdb.username;
          DOCKER_INFLUXDB_INIT_PASSWORD=secrets.influxdb.username;
          DOCKER_INFLUXDB_INIT_ORG=secrets.influxdb.org;
          DOCKER_INFLUXDB_INIT_BUCKET=secrets.influxdb.bucket;
        };
        ports = [ "8086:8086" ];
        volumes = [
          "/mnt/download/influx/influxdb2:/var/lib/influxdb2"
          "/mnt/download/influx/config:/etc/influxdb2"
        ];
      };
      esphome = {
        autoStart = true;
        image = "esphome/esphome:2022.11.1";
        environment = {
          TZ="Europe/Warsaw";
        };
        ports = [
          "6052:6052"
	        "6123:6123"
        ];
        volumes = [
          "/home/peel/wrk/esphome:/config"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          "--privileged"
          "--network=host"
        ];
      };
    };
  };
}
