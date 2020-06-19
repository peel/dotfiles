{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.orther.secrets;
in {
  options.orther.secrets.enable = mkEnableOption "secrets";
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs._1password ];
    nix.extraOptions = ''
     plugin-files = ${pkgs.nix-plugins.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
    '';
  };
}
