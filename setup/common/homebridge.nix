{ config, lib, pkgs, ... }:

let
  image = "oznu/homebridge";
  secret = import <dotfiles/setup/secret/homebridge>;
  config = pkgs.writeText "config.json" (builtins.toJSON {
    bridge = secret.bridge;
    accessories = [
      { accessory = "webostv";
        name = "tv";
        ip = secret.webos.ip;
        mac = secret.webos.mac;
        keyFile = "/homebridge/lgtvkey";
        tvService = true;
        inputs = [
          { appId = "com.webos.app.livetv"; name = "Live TV"; }
          { appId = "com.webos.app.hdmi3"; name = "AppleTV"; }
          { appId = "hbogocev5"; name = "HBO"; }
          { appId = "netflix"; name = "Netflix"; }
          { appId = "com.webos.app.photovideo"; name = "Photos"; }
          { appId = "com.accedo.videa.tvn"; name = "Player.pl"; }
        ];
        volumeControl = false;
        channelControl = false;
      }
      { accessory = "XiaomiRoborockVacuum";
        name = "Roborock";
        ip = secret.roborock.ip;
        token = secret.roborock.token;
      }
    ];
    platforms = [
      # { platform = "HarmonyHubWebSocket"; name = "Harmony Hub"; }
      {
        platform = "ZigBeePlatform";
        port = "/dev/ttyACM0";
        panId = 60666;
        channel = 11;
        disableLed = true;
      }
      {
        platform = "eveatmo";
        name = "eveatmo platform";
        ttl = 540;
        auth = secret.netatmo;
      }
      {
        platform = "Yeelight-Platform";
      }        
    ];
  });
  packages = pkgs.writeText "package.json" (builtins.toJSON {
    config.unsafe-perm = true;
    dependencies = {
      homebridge-eveatmo = "0.4.3";
      # homebridge-harmony = "0.1.5";
      homebridge-xiaomi-roborock-vacuum = "0.4.1";
      homebridge-webos-tv = "1.5.4";
      homebridge-yeelight-platform = "1.2.4";
      homebridge-zigbee-v2 = "0.0.22";
    };
  });
  version = "no-avahi";
in {
  systemd.services.homebridge = {
    enable = true;
    after =  [ "network.target" "docker.service" ];
    wants = [ "network.target" "docker.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.docker ];
    serviceConfig = {
      Restart = "always";
      ExecStartPre = [
        "-${pkgs.docker}/bin/docker stop homebridge"
        "-${pkgs.docker}/bin/docker rm homebridge"
        "-${pkgs.docker}/bin/docker pull ${image}:${version}"
        "${pkgs.coreutils}/bin/mkdir -p /var/lib/homebridge"
        "${pkgs.coreutils}/bin/cp -f ${packages} /var/lib/homebridge/package.json"
        "${pkgs.coreutils}/bin/cp -f ${config} /var/lib/homebridge/config.json"
        "${pkgs.coreutils}/bin/chmod -R +w /var/lib/homebridge"
      ];
      ExecStart = ''
      ${pkgs.docker}/bin/docker run \
        --net=host \
        --device=/dev/ttyACM0 \
        --privileged=true \
        --name=homebridge \
        -e TZ=Europe/Warsaw \
        -e HOMEBRIDGE_CONFIG_UI=1 \
        -e HOMEBRIDGE_INSECURE=1 \
        -e PGID=0 \
        -e PUID=0 \
        -e PACKAGES=make,gcc,g++,python,linux-headers,udev \
        -v /var/lib/homebridge:/homebridge/ \
        ${image}:${version}
      '';
    };
  };
}
  
