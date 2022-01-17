{ config, pkgs, ...}:

{
  environment.systemPackages = pkgs.lib.optionals pkgs.stdenvNoCC.isDarwin [
  ];
}
