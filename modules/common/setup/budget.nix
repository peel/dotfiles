{ config, lib, pkgs, ...}:

let secrets = import ./secrets.nix;
    cfg = config.peel.budget;
in {
  options.peel.budget = {
    enable = lib.mkEnableOption "budget";
    backup = lib.mkOption {
      default = true;
    };
    data = lib.mkOption {
      default = "/var/lib/actual";
    };
    port = lib.mkOption {
      default = "5006";
    };
  };

  config = {
    virtualisation.oci-containers.containers.budget = lib.mkIf cfg.enable {
      autoStart = true;
      image = "actualbudget/actual-server";
      environment = {
      };
      volumes = [
        "${cfg.data}:/data"
      ];
      ports = [
        "5006:5006"
      ];
    };

  };
}
