{ config, pkgs, lib, ...}:

{
  homebrew = {
    enable = true;
    casks = [
      "dash"
      "plexamp"
      "vmware-fusion"
      "zoom"
    ];
    masApps = {
      "Slack" = 803453959;
      "1Password" = 1333542190;
      "Endel" = 1484348796;
      "PopClip" = 445189367;
      "HomeAssistant" = 1099568401;
    };
    cleanup = "zap";
  };
}
