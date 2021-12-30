{ config, pkgs, lib, ...}:

{
  homebrew = {
    enable = true;
    casks = [
      "dash"
      "slack"
      "vmware-fusion"
      "zoom"
    ];
    masApps = {
      "1Password" = 1107421413;
    };
  };
}
