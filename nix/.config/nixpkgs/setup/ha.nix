{ config, lib, pkgs, ... }:

let
  nixpkgs = import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/cc1d13ae0f0d7c2bb6d6be9e64349a628ca0512f.tar.gz";
              sha256 = "0j8ml871rikpd243dy23r6k9cw5mq3z560yd2s3s454k7pdk50i6";
            }) { config = { }; };
  hassCfg = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "peel";
    repo = "hassio";
    sha256 = "13knldz56cw1mandsrq47p1p5xm2yzqpm5qbjbxy47d9av9nx2m6";
    rev = "0.0.2";
  }) { pkgs = nixpkgs; };
  domain = builtins.readFile (./secret/domain);
  orgdomain = builtins.readFile (./secret/org.domain);
  version = "latest";
in {
  services.ddclient = {
    enable = true;
    protocol = "duckdns";
    domains = [ "${domain}" ];
    password = builtins.readFile (./secret/ddclient.password);
  };
  services.nginx = {
    enable = true;
    virtualHosts."${domain}.duckdns.org" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://localhost:8123";
        proxyWebsockets = true;
      };
    };
    virtualHosts."ha.${orgdomain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://localhost:8123";
        proxyWebsockets = true;
      };
    };
  };
  systemd.services.hassCfg = {
    after = [ "network.target" ];
    before = [ "home-assistant.target" ];
    wants = [ "home-assistant.target" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      mkdir -p /var/lib/hass
      ls ${hassCfg}/var/lib/hass
      cp -r ${hassCfg}/var/lib/hass/* /var/lib/hass/
      chmod -R a+x+w /var/lib/hass
    '';
  };
  
  systemd.services.home-assistant = {
    after =  [ "hassCfg.target" "network.target" "docker.service" ];
    wants = [ "hassCfg.target" "network.target" "docker.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.docker ];
    serviceConfig = {
      Restart = "always";
      ExecStartPre = [
        "-${pkgs.docker}/bin/docker stop home-assistant"
        "-${pkgs.docker}/bin/docker rm home-assistant"
        "${pkgs.docker}/bin/docker pull homeassistant/home-assistant:${version}"
      ];
      ExecStart = ''
        ${pkgs.docker}/bin/docker run --rm \
          --name="home-assistant" \
          -v /var/lib/hass:/config \
          -v /etc/localtime:/etc/localtime:ro \
          --net=host \
          homeassistant/home-assistant:${version}
      '';
    };
  };
}
