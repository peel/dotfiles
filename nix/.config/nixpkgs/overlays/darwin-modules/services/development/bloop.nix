{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.bloop;
in {
  options = {
    
    services.bloop.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable Bloop build server.";
    };
    
    # services.bloop.package = {
    #   type = types.path;
    #   default = pkgs.bloop;
    #   description = "Bloop package to use.";
    # };
    
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.bloop];
    launchd.user.agents.bloop = {
      serviceConfig.ProgramArguments = [ "${pkgs.bloop}/bin/blp-server" ];
      serviceConfig.KeepAlive = true;
      serviceConfig.ProcessType = "Interactive";
    };
  };
  
}
