{ config, lib, pkgs, ... }:

let
  nixpkgs = import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/cc1d13ae0f0d7c2bb6d6be9e64349a628ca0512f.tar.gz";
              sha256 = "0j8ml871rikpd243dy23r6k9cw5mq3z560yd2s3s454k7pdk50i6";
            }) { config = { }; };
  hassCfg = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "peel";
    repo = "hassio";
    sha256 = "14xkjigbfyh1khlyn608j41gkkf5fc7qpj1ip1hllk2kcz5h6yz8";
    rev = "0.0.3";
  }) { pkgs = nixpkgs; };
  domain = builtins.readFile (./secret/domain);
  orgdomain = builtins.readFile (./secret/org.domain);
  version = "0.84.6";
in {
  services.ddclient = {
    enable = true;
    protocol = "duckdns";
    domains = [ "${domain}" ];
    password = builtins.readFile (./secret/ddclient.password);
  };
  services.fail2ban.enable = true;
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
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
    enable = true;
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
    enable = true;
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
  
