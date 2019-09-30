{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.peel.secrets;
in {
  options.peel.secrets.enable = mkEnableOption "secrets";
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs._1password ];
    nix.extraOptions = ''
     plugin-files = ${pkgs.nix-plugins.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
    '';
  };
}
