{ config, lib, pkgs, ...}:

let secrets = import ./secrets.nix;
    cfg = config.peel.arr;
    credentialsDir = "/run/secrets/credentials";
in {
  options.peel.arr = {
    enable = lib.mkEnableOption "arr";
    downloads = lib.mkEnableOption "downloads";
    
    downloadsDir = lib.mkOption {
      default = "/mnt/download";
    };
    openFirewall = lib.mkOption {
      default = true;
    };
    data = lib.mkOption {
      default = "/var/lib";
    };
  };

  config = {
    services.sonarr = lib.mkIf cfg.enable {
      enable = true;
      user = "root";
      group = "root";
      openFirewall = cfg.openFirewall;
    };
    services.radarr = lib.mkIf cfg.enable {
      enable = true;
      user = "root";
      group = "root";
      openFirewall = cfg.openFirewall;
    };
    services.lidarr = lib.mkIf cfg.enable {
      enable = true;
      user = "root";
      group = "root";
      openFirewall = cfg.openFirewall;
    };
    services.prowlarr = lib.mkIf cfg.downloads {
      enable = true;
      openFirewall = cfg.openFirewall;
    };
		services.nzbget = lib.mkIf cfg.downloads {
		  enable = true;
			user = "root";
			group = "root";
			settings = {
			  DestDir = cfg.downloadsDir;
			};
		};
    services.transmission = lib.mkIf cfg.downloads {
      enable = true;
      user = "root";
      group = "root";
      webHome = pkgs.flood-for-transmission;
      settings = {
        download-dir = cfg.downloadsDir;
        incomplete-dir = "${cfg.downloadsDir}/.incomplete";
        incomplete-dir-enabled = true;
        rpc-port = 9091;
        rpc-whitelist = "127.0.0.1,192.168.1.*,100.120.149.42";
        rpc-host-whitelist = "*";
        ratio-limit = 1;
        ratio-limit-enabled = true;
        rpc-bind-address = "0.0.0.0";  # web server
        openFirewall = true;
        openPeerPorts = true;
        openRPCPort = true;
      };
    };
    systemd.tmpfiles.rules =
    let
      user = "root";
      group = "root";
      downloadDir = config.services.transmission.settings.download-dir;
    in
      [ #Type Path                      Mode UID     GID      Age Argument
        "d    ${downloadDir}            0755 ${user} ${group} -   -"
      ];

    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPorts = [
        5055 # overseer
      ];
    };
    virtualisation.oci-containers.containers = {
      overseerr = {
        image = "ghcr.io/linuxserver/overseerr";
        volumes = [
          "${cfg.data}/overseerr:/config"
        ];
        extraOptions = [
          # Acess to Radarr / Sonarr
          "--network=host"
        ];
      };
    };
  };
}
