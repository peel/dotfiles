{ config, pkgs, lib, ...}:

let
  isDarwinArm64 = pkgs.stdenvNoCC.isDarwin && pkgs.stdenvNoCC.isAarch64;
in {
  homebrew = {
    enable = true;
    casks = [
      "1password-beta"
      "alfred"
      "dash"
      "docker"
      "openaudible"
      "plexamp"
      "utm"
      "vmware-fusion"
      "zoom"
    ] ++ lib.optionals (!isDarwinArm64) ["vmware-fusion"];
    masApps = {
      # "Slack" = 803453959;
      # "1Password" = 1333542190;
      "Endel" = 1484348796;
      "PopClip" = 445189367;
      "Tailscale" = 1475387142;
    };
    cleanup = "zap";
  } // lib.optionals isDarwinArm64 {
    extraConfig = ''
        tap "homebrew/cask-versions"
         # cask "vmware-fusion-tech-preview"
    '';
  };
}
