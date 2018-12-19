{ config, lib, pkgs, ... }:

let
  nixpkgs = import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/cc1d13ae0f0d7c2bb6d6be9e64349a628ca0512f.tar.gz";
              sha256 = "0j8ml871rikpd243dy23r6k9cw5mq3z560yd2s3s454k7pdk50i6";
            }) { config = { }; };
  hass = nixpkgs.home-assistant.overrideAttrs(oldAttrs: rec {
      doInstallCheck = false;
    });
in {
  networking.firewall.enable = false;
  services.home-assistant = {
    enable = true;
    openFirewall = true;
    autoExtraComponents = false;
    package = hass.override {
      skipPip = true;
      packageOverrides = self: super: {
            websockets = super.websockets.overrideAttrs(o: {
              version = "6.0";
              src = super.fetchPypi {
                pname = "websockets";
                version = "6.0";
                sha256 = "8f3b956d11c5b301206382726210dc1d3bee1a9ccf7aadf895aaf31f71c3716c";
              };
            });
            sqlalchemy = super.websockets.overrideAttrs(o: {
              version = "1.2.14";
              src = super.fetchPypi {
                pname = "SQLAlchemy";
                version = "1.2.14";
                sha256 = "9de7c7dabcf06319becdb7e15099c44e5e34ba7062f9ba10bc00e562f5db3d04";
              };
            });
            android_backup = super.buildPythonPackage rec {
              pname = "android_backup";
              version = "0.2.0";
              src = super.fetchPypi {
                inherit pname version;
                sha256 = "15wb2lyjj2fpf7bhvmgpqn0mglsjj11zfvbjycx7mnidisgnljw6";
              };
              propagatedBuildInputs = with super; [  ];
              doCheck = false;
            };
            pretty_cron = super.buildPythonPackage rec {
              pname = "pretty-cron";
              version = "1.2.0";
              src = super.fetchPypi {
                inherit pname version;
                sha256 = "163cr2jsyy8qrpd9vbgi38sqk2110523hcz7d3mq10wzabp4xq0m";
              };
              propagatedBuildInputs = with super; [
                atomicwrites
                configparser
                docutils
                enum34
                flake8
                funcsigs
                # futures # not supported
                isort
                mccabe
                # modernize # miss
                more-itertools
                # multilint # miss
                pathlib2
                pluggy
                py
                pycodestyle
                pyflakes
                pygments
                pytest
                scandir
                six
                ] ++ [self.attrs];
              doCheck = false;
            }; 
           base36 = super.buildPythonPackage rec {
              pname = "base36";
              version = "0.1.1";
              src = super.fetchPypi {
                inherit pname version;
                sha256 = "15hs1h3ybvyi3z151a2wzwr653b3z56ha0hi9byxb6s9qn1if8kg";
              };
              propagatedBuildInputs = with super; [ ];
              doCheck = false;
            };
            curve25519-donna = super.buildPythonPackage rec {
              pname = "curve25519-donna";
              version = "1.3";
              src = super.fetchPypi {
                inherit pname version;
                sha256 = "1w0vkjyh4ki9n98lr2hg09f1lr1g3pz48kshrlic01ba6pasj60q";
              };
              propagatedBuildInputs = with super; [ ];
              doCheck = false;
            };
      };
      extraPackages = ps:
        let
          pps = ps // {
            miio = ps.buildPythonPackage rec {
              pname = "python-miio";
              version = "0.4.4";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "1y7vrsgdcf77msa57ybnv6yjr03bvk6xb6hfxq94vms9820cn8ni";
              };
              propagatedBuildInputs = with ps; [
                click
                cryptography
                pretty_cron
                construct
                zeroconf
                attrs
                typing  # for py3.4 support
                pytz  # for tz offset in vacuum
                appdirs  # for user_cache_dir of vacuum_cli
                tqdm
                netifaces # for updater
                android_backup
              ];
              doCheck = false;
            };
            HAP-python = ps.buildPythonPackage rec {
              pname = "HAP-python";
              version = "2.4.1";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "0dnpxcva3baxb3cjxjhr8ib7q6vslgc07vcjbkx8f7ajqc3fm1wx";
              };
              propagatedBuildInputs = with ps; [ ecdsa ed25519 pycryptodome pyqrcode tlslite-ng zeroconf curve25519-donna base36];
              doCheck = false;
            };
            gTTS-token = ps.buildPythonPackage rec {
              pname = "gTTS-token";
              version = "1.1.3";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "1i8jq15ml5wmvms9walv7j23vw40nva1m4zgjx9j6gw1bfl1js4x";
              };
              propagatedBuildInputs = with ps; [ requests ed25519 pycryptodome pyqrcode tlslite-ng zeroconf curve25519-donna base36];
              doCheck = false;
            };
            pyhap = ps.buildPythonPackage rec {
              pname = "pyhap";
              version = "0.1.1";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "0wbwmbnvb4r2sjkxl556xbv9f38bz5vf56n8l5zzb7db0dqd77ns";
              };
              propagatedBuildInputs = with ps; [ cryptography ed25519 zeroconf ];
              doCheck = false;
            };
            pyharmony = ps.buildPythonPackage rec {
              pname = "pyharmony";
              version = "1.0.21";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "18r6zdfyq3rnv10g72zlxvjyj9rvhxypwk31yx6341pbg79vjpvi";
              };
              propagatedBuildInputs = with ps; [ requests sleekxmpp ];
              postPatch = ''
                sed -i -e "/argparse/d" requirements.txt
              '';
              doCheck = false;
            };
            pylgtv = ps.buildPythonPackage rec {
              pname = "pylgtv";
              version = "0.1.9";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "0k2cj33mnfp914kvj698ldxw2807f6z1l1jr1h99h1xfdwrkz80f";
              };
              propagatedBuildInputs = with ps; [ asyncio websockets ];
              doCheck = false;
            };
            pysonos = ps.buildPythonPackage rec {
              pname = "pysonos";
              version = "0.0.5";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "0ad8rqm7bid72s5d5969rr31014gnz2cgznrl3i7cmsxg6ycm8b7";
              };
              propagatedBuildInputs = with ps; [ requests xmltodict ifaddr ];
              doCheck = false;
            };
            pynetgear = ps.buildPythonPackage rec {
              pname = "pynetgear";
              version = "0.5.1";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "0y84wprlkb21libj17fhk7h6s3yjryjhc4nw3xmdbnp130rpfmm8";
              };
              propagatedBuildInputs = with ps; [ requests ];
              doCheck = false;
            };
            yeelight = ps.buildPythonPackage rec {
              pname = "yeelight";
              version = "0.4.3";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "1mpcgg2qpwasvxkqz051jns9yqm7ajjcypak8a172innvs7s5y7h";
              };
              propagatedBuildInputs = with ps; [ future enum-compat ];
              doCheck = false;
            };
            aiohue = ps.buildPythonPackage rec {
              pname = "aiohue";
              version = "1.5.0";
              src = ps.fetchPypi {
                inherit pname version;
                sha256 = "14j0xagx61vacvf2lmq0317s1p87dbpc4yqxx3m97hfnsaqwq04k";
              };
              propagatedBuildInputs = with ps; [ aiohttp ];
              doCheck = false;
            };
          };
        in with pps; [
          HAP-python
          sqlalchemy
          websockets
          pip
          gTTS-token
          aiohue
          miio
          pyhap
          pyharmony
          pylgtv
          pysonos
          pynetgear
          yeelight
      ];
      extraComponents = [
        "api"
        "auth"
        "automation"
        "cloud"
        "config"
        "config.auth"
        # "config.auth_provider_homeassistant"
        "config.automation"
        "config.config_entries"
        "config.core"
        "config.customize"
        "config.device_registry"
        "config.entity_registry"
        "config.group"
        "config.script"
        "conversation"
        "deconz"
        "device_tracker"
        "device_tracker.netgear"
        "discovery"
        "emulated_hue"
        "frontend"
        "group"
        "hassio"
        "history"
        # "homekit"
        "http"
        "ios"
        "light"
        "light.yeelight"
        "logbook"
        "lovelace"
        "map"
        "media_player"
        "media_player.sonos"
        "media_player.webostv"
        "notify"
        "notify.ios"
        "onboarding"
        "recorder"
        "remote"
        "remote.harmony"
        "script"
        "sensor"
        "sensor.ios"
        "sensor.yr"
        "sonos"
        "sun"
        "system_log"
        "tts"
        "updater"
        "vacuum"
        "vacuum.xiaomi_miio"
        "wake_on_lan"
        "websocket_api"
        "zeroconf"
        "zone"
      ];
    };
  };
}
