{ config, pkgs, ...}:

{
  services.bloop.install = true;
  environment.systemPackages = with pkgs; [];
}
