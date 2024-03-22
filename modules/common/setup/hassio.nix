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
      default = "2024.2.1";
    };
    plex = lib.mkOption {
      default = pkgs.plex;
    };
    actual = {
      enable = lib.mkEnableOption "actual";
      backup = lib.mkOption {
        default = true;
      };
      data = lib.mkOption {
        default = "/var/lib/actual";
      };
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

    systemd.services.esphome.serviceConfig.ProcSubset = lib.mkForce "all"; # fixed in 23.11
    services.esphome = {
      enable = true;
      allowedDevices = [
        "/dev/serial/by-id/usb-Espressif_USB_JTAG_serial_debug_unit_34:B4:72:87:21:54-if00"
      ];
      address = "0.0.0.0";
      openFirewall = true;
    };

    services.go2rtc = {
      enable = true;
      settings = {
        streams = secrets.streams;
        rtsp.listen = "0.0.0.0:8555";
      };
    };
    services.plex = {
      enable = true;
      package = cfg.plex;
      openFirewall = true;
      user = "root";
      extraPlugins = [
        (builtins.path {
          name = "Audnexus.bundle";
          path = pkgs.fetchFromGitHub {
            owner = "djdembeck";
            repo = "Audnexus.bundle";
            rev = "v1.3.1";
            sha256 = "sha256-HgbPZdKZq3uT44n+4owjPajBbkEENexyPwkFuriiqU4=";
          };
        })
      ];
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
          port = "/dev/ttyACM2";
        };
        # groups = {
        #   "1" = {
        #     friendly_name = "Przedpokój";
        #     devices = [  ];
        #   };
        #   "2" = {
        #     friendly_name = "Gabinet Track";
        #     devices = [  ];
        #   };
        # };
      };
    };
    services.nats = {
      enable = true;
      jetstream = true;
      settings = {
        http_port = 8222;
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
      '' + lib.optionalString (cfg.actual.enable && cfg.actual.backup) ''
      ${pkgs.gnutar}/bin/tar -cvf hassio-backup.tar /home/peel/wrk/hassio
      ${pkgs.coreutils}/bin/mv hassio-backup.tar /mnt/download
    '';
    };
    virtualisation.oci-containers.containers = {
      # expose apcupsd to mqtt
      apcupsd2mqtt = {
        autoStart = true;
        image = "ghcr.io/joeyeamigh/apcupsd-mqtt-exporter:v0.2.3";
        environment = {
          RUST_LOG = "info";
          APCUPSD_HOST = "172.17.0.1";                    # host running apcupsd
          APCUPSD_PORT = "3551";                          # port apcupsd is listening on
          APCUPSD_STRIP_UNITS = "true";                   # strip units from apcupsd values
          APCUPSD_POLL_INTERVAL = "10";                   # seconds between polling apcupsd
          APCUPSD_POLL_TIMEOUT = "5";                     # seconds between polling apcupsd
          MQTT_HOST = "172.17.0.1";                       # host running MQTT broker
          MQTT_PORT = "1883";                             # port MQTT broker is listening on
          MQTT_USERNAME = secrets.mqtt.username;          # MQTT username (optional)
          MQTT_PASSWORD = secrets.mqtt.password;          # MQTT password (optional)
          MQTT_CLIENT_ID = "upcupsd";                     # MQTT client ID
          MQTT_TOPIC = "homeassistant/sensor/ups";        # MQTT topic to publish to
          MQTT_SUFFIX = "status";                         # MQTT topic suffix (optional)
          HOME_ASSISTANT_MODE = "true";                   # publish MQTT messages in Home Assistant-compatible JSON
          HOME_ASSISTANT_UUID_PREFIX = "apcupsd_";        # prefix for Home Assistant UUIDs
        };
        extraOptions = [
          "--network=host"
        ];
      };
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
      actual = lib.mkIf cfg.actual.enable {
        autoStart = true;
        image = "actualbudget/actual-server";
        environment = {
        };
        volumes = [
          "${cfg.actual.data}:/data"
        ];
        ports = [
          "5006:5006"
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
          "--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00"
          "--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2686951-if00"
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
    };
  };
}
