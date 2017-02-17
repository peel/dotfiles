{ config, pkgs, ... }:

{
  system = {
   defaults.dock = {
     autohide = true;
     orientation = "bottom";
     showhidden = true;
   }
  }
}