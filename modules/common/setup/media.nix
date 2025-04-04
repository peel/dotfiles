{ config, lib, pkgs, ...}:

let secrets = import ./secrets.nix;
    cfg = config.peel.media;
    credentialsDir = "/run/secrets/credentials";
    dataDirPrefix = "/var/lib";
in {
  options.peel.media = {
    enable = lib.mkEnableOption "media";
    openFirewall = lib.mkOption {
       default = true;
    };
    plex =  {
      enable = lib.mkEnableOption "plex";
      package = lib.mkOption {
        default = pkgs.plex;
      };
    };
    jellyfin = {
      enable = lib.mkEnableOption "jellyfin";
      package = lib.mkOption {
        default = pkgs.jellyfin;
      };
    };
    books = {
      enable = lib.mkEnableOption "books";
      package = lib.mkOption {
        default = pkgs.audiobookshelf;
      };
      data = lib.mkOption {
        default = "${dataDirPrefix}/audiobookshelf";
      };
    };
    music = {
      enable = lib.mkEnableOption "music";
      data = lib.mkOption {
        default = "/mnt/music";
      };
      navidromePackage = lib.mkOption {
        default = pkgs.navidrome;
      };
      navidromeExtraSettings = lib.mkOption {
        default = {};
      };
    };
  };

  config = {
    hardware.opengl = {
      driSupport32Bit = true;
      extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl ];
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva vaapiIntel libvdpau-va-gl vaapiVdpau ];
    };

    services.plex = lib.mkIf cfg.plex.enable {
      enable = true;
      package = cfg.plex.package;
      openFirewall = cfg.openFirewall;
      user = "root";
      accelerationDevices = [ "*" ];
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

    services.jellyfin = lib.mkIf cfg.jellyfin.enable {
      enable = true;
      user = "root";
      group = "root";
      openFirewall = cfg.openFirewall;
    };

    services.gonic = let multi-mode = "delim ;"; in lib.mkIf cfg.music.enable rec {
      enable = true;
      settings = {
        listen-addr = "0.0.0.0:4000";
        music-path = cfg.music.data;
        playlists-path = "${cfg.music.data}/.playlists";
        podcast-path = "/dev/null";
        scan-interval = 60; # Minutes
        multi-value-genre = multi-mode;
        multi-value-artist = multi-mode;
        multi-value-album-artist = multi-mode;
      };
    };

    services.navidrome = lib.mkIf cfg.music.enable {
      enable = true;
      package = cfg.music.navidromePackage;
      settings = {
        Address = "0.0.0.0";
        Port = 4533;
        MusicFolder = cfg.music.data;
        "Scanner.Schedule" = "@every 20m";
        # make it big to work well with music assistant and sonos
        TranscodingCacheSize = "10G";
        # https://www.navidrome.org/docs/usage/configuration-options/#advanced-configuration
        CoverArtPriority = "cover.*, embedded, front.*, folder.*";
        DefaultTheme = "nord";
        EnableSharing = true;
        EnableMediaFileCoverArt = false;
        EnableCoverAnimation = false;
        EnableTranscodingConfig = true;
      } // cfg.music.navidromeExtraSettings;
    };

    services.audiobookshelf = lib.mkIf cfg.books.enable {
      enable = true;
      user = "root";
      group = "root";
      host = "0.0.0.0";
      port = 10802;
      openFirewall = cfg.openFirewall;
      package = cfg.books.package;
    };

  };
}
