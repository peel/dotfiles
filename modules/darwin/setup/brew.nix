{ config, pkgs, lib, ...}:

let
  isDarwinArm64 = pkgs.stdenvNoCC.isDarwin && pkgs.stdenvNoCC.isAarch64;
in {
  homebrew = {
    enable = true;
    casks = [
      "dash"
      "docker"
      "plexamp"
      "zoom"
      "1password-beta"
    ] ++ lib.optionals (!isDarwinArm64) ["vmware-fusion"];
    masApps = {
      "Slack" = 803453959;
      # "1Password" = 1333542190;
      "Endel" = 1484348796;
      "PopClip" = 445189367;
    };
    cleanup = "zap";
  } // lib.optionals isDarwinArm64 {
    extraConfig = ''
        tap "homebrew/cask-versions"
        cask "vmware-fusion-tech-preview"
    '';
  };
}
