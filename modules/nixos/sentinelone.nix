{ config, pkgs, lib, ...}:

with lib;

let
  cfg = config.peel.sentinelone;
in {

  options.peel.sentinelone = {
    enable = mkEnableOption "sentinelone";
    # TODO use mkPackageOption
    package = mkOption {
      default = pkgs.sentinelone;
    };
  };

  config = {
    systemd.services.sentinelone = mkIf (cfg.enable) {
      enable = true;
      description = "sentinelone agent";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        User = "sentinelone";
        Group = "sentinelone";
        Type = "oneshot";
        ExecStart = ''
         ${pkgs.sentinelone}/bin/sentinelctl control start
       '';
        ExecStop = ''
         ${pkgs.sentinelone}/bin/sentinelctl control stop
       '';
        Restart = "on-failure";
        RestartSec = 2;
      };
    };

    users.extraUsers.sentinelone = mkIf (cfg.enable) {
      description = "User for sentinelone";
      isNormalUser = true;
      shell = "${pkgs.coreutils}/bin/true";
      home = "/home/sentinelone";
      extraGroups = [ "wheel" ];
    };

    users.groups.sentinelone.members = mkIf (cfg.enable != null) [
      "sentinelone"
    ];
  };
}
