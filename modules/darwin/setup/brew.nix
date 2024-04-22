{ config, pkgs, lib, ...}:

let
  isDarwinArm64 = pkgs.stdenvNoCC.isDarwin && pkgs.stdenvNoCC.isAarch64;
in {
  homebrew = {
    enable = true;
    taps = [
      {
        name = "akirakyle/homebrew-qemu-virgl";
        clone_target = "https://github.com/akirakyle/homebrew-qemu-virgl.git";
        force_auto_update = true;
      }
    ];
    casks = [
      "1password"
      "alfred"
      "dash"
      "docker"
      "openaudible"
      "plexamp"
      "utm"
      "vmware-fusion"
      "zoom"
    ];
    brews = [
      "akirakyle/qemu-virgl/qemu-virgl"
    ];
    masApps = {
      # "Slack" = 803453959;
      # "1Password" = 1333542190;
      "Endel" = 1484348796;
      "PopClip" = 445189367;
      "Tailscale" = 1475387142;
    };
    onActivation.cleanup = "zap";
  };
}
