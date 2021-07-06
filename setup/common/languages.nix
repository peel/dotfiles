{ config, pkgs, ...}:

{
  services.bloop.install = true;
  environment.systemPackages = pkgs.lib.optionals pkgs.stdenvNoCC.isDarwin [
    pkgs.Dash
    # pkgs.Docker
  ];
}
