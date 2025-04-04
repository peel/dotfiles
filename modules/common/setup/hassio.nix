{ config, lib, pkgs, ... }:

let
  secrets = import ./secrets.nix;
  cfg = config.peel.hassio;
  credentialsDir = "/run/secrets/credentials";
in {
  options.peel.hassio = {
    enable = lib.mkEnableOption "hassio";
    zigbee2mqtt = lib.mkOption {
      default = pkgs.zigbee2mqtt;
    };
    home-assistant = lib.mkOption {};
    music-assistant = {
      enable = lib.mkEnableOption "music-assistant";
      data = lib.mkOption {
        default = "/var/lib/music-assistant";
      };
      media = lib.mkOption {};
      environment = {
        LOG_LEVEL = "warning";
      };
    };
    matter-server = {
      enable = lib.mkEnableOption "matter-server";
      data = lib.mkOption {
        default = "/var/lib/matter";
      };
      version = lib.mkOption {
        default = "7.0.1";
      };
    };
    govee2mqtt = {
      enable = lib.mkEnableOption "govee2mqtt";
      version = lib.mkOption {
        default = "latest";
      };
    };
    dashboard = {
      enable = lib.mkEnableOption "dashboard";
    };
    scrypted = {
      enable = lib.mkEnableOption "scrypted";
      data = lib.mkOption {
        default = "/var/lib/scrypted";
      };
    };
  };
  config = {
    # services.mealie = {
    #   enable = true;
    #   port = 9099;
    #   credentialsFile = "${credentialsDir}/mealie.env";
    # };
    services.homepage-dashboard = {
      enable = cfg.dashboard.enable;
      listenPort = 8082;
      openFirewall = true;
      settings = {};
      services = {
        "Money" = [
          {"Budget" = {
            description = "Budget";
            href = "https://budget.fff666.org/";
          };}
        ];
        "Home" = [
          {"Home Assistant" = {
            description = "Home Assistant";
            href = "https://nuke.local:8123";
          };}
          {"z2m" = {
            description = "Zigbee2MQTT";
            href = "http://nuke.local:8124/#/";
          };}
          {"Networking" = {
            description = "Unifi Network";
            href = "https://192.168.1.1/network/default/dashboard";
          };}
          {"NAS" = {
            description = "Synology";
            href = "https://nas.local:5001/#/signin/password";
          };}
          {"tailscale" = {
            description = "";
            href = "https://login.tailscale.com/admin/machines";
          };}
        ];
        "Sports" = [
          {"TrainHeroic" = {
            description = "";
            href = "https://athlete.trainheroic.com/#/training?pwId=68152701";
          };}
          {"btwb" = {
            description = "";
            href = "https://www.btwb.com/whiteboard";
          };}
          {"whoop" = {
            description = "";
            href = "https://app.whoop.com/athlete/11147439/1d/today";
          };}
          {"mobility manual" = {
            description = "";
            href = "https://members.mobilitymanual.com/?sfwd-courses=lifters-mobility-manual";
          };}
        ];
        "Knowledge" = [
          {"Books" = {
            description = "Books Manager";
            href = "http://192.168.1.220:8083/";
          };}
          {"Reader" = {
            description = "Readwise Reader";
            href = "https://read.readwise.io";
          };}
          {"Readwise" = {
            description = "Readwise";
            href = "https://readwise.io/dashboard";
          };}
        ];
        "Media" = [
          {"Video" = {
            description = "Books Manager";
            href = "https://px.fff666.org";
          };}
          {"Music" = {
            description = "Navidrome";
            href = "https://m.fff666.org/app/#/album/recentlyAdded?sort=recently_added&order=DESC&filter=%7B%7D";
          };}
        ];
      };
      widgets = [
        {
          resources = {
            cpu = true;
            disk = "/";
            memory = true;
          };
        }
        {
          search = {
            provider = "duckduckgo";
            target = "_blank";
          };
        }
      ];
      docker = {
        socket = "/var/run/docker.sock";
      };
    };
    services.paperless = {
      enable = false;
      passwordFile = "${credentialsDir}/paperless.env";
      port = 28981;
      dataDir = "/var/lib/paperless";
      mediaDir = "/var/lib/paperless/media";
      consumptionDir = "/var/lib/paperless/in";
      consumptionDirIsPublic = true;
      address = "0.0.0.0";
    };
    # systemd.services.esphome.serviceConfig.ProcSubset = lib.mkForce "all"; # fixed in 23.11
    # services.esphome = {
    #   enable = true;
    #   allowedDevices = [
    #     "/dev/serial/by-id/usb-Espressif_USB_JTAG_serial_debug_unit_34:B4:72:87:21:54-if00"
    #   ];
    #   address = "0.0.0.0";
    #   openFirewall = true;
    # };

    services.go2rtc = {
      enable = true;
      settings = {
        streams = secrets.streams;
        rtsp.listen = "0.0.0.0:8555";
      };
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
    # systemd.timers.hassio-backup = {
    #   wantedBy = [ "timers.target" ];
    #   timerConfig.OnCalendar = "*-*-* 4:00:00"; # everyday at 4AM
    # };
    # systemd.services.hassio-backup = {
    #   description = "Backup hassio directory to nas";
    #   after = [ "network-pre.target" "docker-home-assistant.service" ];
    #   wants = [ "network-pre.target" "docker-home-assistant.service" ];
    #   wantedBy = [ "timers.target" ];
    #   serviceConfig.Type = "oneshot";
    #   serviceConfig.WorkingDirectory = "/home/peel/wrk/hassio";
    #   script = with pkgs; ''
    #   ${pkgs.gnutar}/bin/tar -cvf hassio-backup.tar /home/peel/wrk/hassio
    #   ${pkgs.coreutils}/bin/mv hassio-backup.tar /mnt/download
    #   '' + lib.optionalString (cfg.actual.enable && cfg.actual.backup) ''
    #   ${pkgs.gnutar}/bin/tar -cvf hassio-backup.tar /home/peel/wrk/hassio
    #   ${pkgs.coreutils}/bin/mv hassio-backup.tar /mnt/download
    # '';
    # };
    virtualisation.oci-containers.containers = {
      # expose apcupsd to mqtt
      # apcupsd2mqtt = {
      #   autoStart = true;
      #   image = "ghcr.io/joeyeamigh/apcupsd-mqtt-exporter:v0.2.3";
      #   environment = {
      #     RUST_LOG = "info";
      #     APCUPSD_HOST = "172.17.0.1";                    # host running apcupsd
      #     APCUPSD_PORT = "3551";                          # port apcupsd is listening on
      #     APCUPSD_STRIP_UNITS = "true";                   # strip units from apcupsd values
      #     APCUPSD_POLL_INTERVAL = "10";                   # seconds between polling apcupsd
      #     APCUPSD_POLL_TIMEOUT = "5";                     # seconds between polling apcupsd
      #     MQTT_HOST = "172.17.0.1";                       # host running MQTT broker
      #     MQTT_PORT = "1883";                             # port MQTT broker is listening on
      #     MQTT_USERNAME = secrets.mqtt.username;          # MQTT username (optional)
      #     MQTT_PASSWORD = secrets.mqtt.password;          # MQTT password (optional)
      #     MQTT_CLIENT_ID = "upcupsd";                     # MQTT client ID
      #     MQTT_TOPIC = "homeassistant/sensor/ups";        # MQTT topic to publish to
      #     MQTT_SUFFIX = "status";                         # MQTT topic suffix (optional)
      #     HOME_ASSISTANT_MODE = "true";                   # publish MQTT messages in Home Assistant-compatible JSON
      #     HOME_ASSISTANT_UUID_PREFIX = "apcupsd_";        # prefix for Home Assistant UUIDs
      #   };
      #   extraOptions = [
      #     "--network=host"
      #   ];
      # };
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
      music-assistant = lib.mkIf cfg.music-assistant.enable {
        autoStart = true;
        image = "ghcr.io/music-assistant/server:beta";
        volumes = [
          "${cfg.music-assistant.data}:/data"
        ] ++ lib.optionals (cfg.music-assistant.media != "") [ "${cfg.music-assistant.media}:/media" ];
        extraOptions = [
          "--network=host"
          "--privileged"
        ];
      };
      matter-server = lib.mkIf cfg.matter-server.enable {
        image = "ghcr.io/home-assistant-libs/python-matter-server:${cfg.matter-server.version}";
        volumes = [
          "hass-matter:/data"
          "/run/dbus:/run/dbus:ro"
        ];
        extraOptions = [
          "--network=host"
        ];
      };
      govee2mqtt = lib.mkIf cfg.govee2mqtt.enable {
        image = "ghcr.io/wez/govee2mqtt:latest:${cfg.matter-server.version}";
        environment = {
          "GOVEE_EMAIL" = "";
          "GOVEE_PASSWORD" = "";
          "GOVEE_API_KEY" = "";
          "GOVEE_MQTT_HOST" = "localhost";
          "GOVEE_MQTT_PORT" = "1883";
          "GOVEE_TEMPERATURE_SCALE" = "C";
          "TZ" = "Europe/Berlin";
        };
        extraOptions = [
          "--network=host"
        ];
      };
      scrypted = lib.mkIf cfg.scrypted.enable {
        autoStart = true;
        image = "ghcr.io/koush/scrypted";
        environment = {
        };
        extraOptions = [
          "--network=host"
        ];
        volumes = [
          "/var/run/dbus:/var/run/dbus:ro"
          "/var/run/avahi-daemon/socket:/var/run/avahi-daemon/socket"
          "${cfg.scrypted.data}:/server/volume"
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
          # "/dev/ttyACM0:/dev/ttyACM0:ro"
          # "/dev/ttyACM1:/dev/ttyACM1:ro"
          "/dev/ttyACM1:/dev/ttyACM1:ro"
          "/var/run/dbus:/var/run/dbus:ro"
        ];
        ports = [
          "8123:8123"
        ];
        extraOptions = [
          "--privileged"
          "--network=host"
          "--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2256895-if00"
          #"--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2686951-if00"
          "--device=/dev/ttyACM2:/dev/ttyACM2"
          # "--device=/dev/ttyACM1:/dev/ttyACM1"
          # "--device=/dev/ttyACM0:/dev/ttyACM0"
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
      it-tools = {
        autoStart = true;
        image = "corentinth/it-tools:latest";
        ports = [
          "9090:80"
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
