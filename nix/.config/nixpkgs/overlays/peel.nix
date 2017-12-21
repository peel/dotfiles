self: super:
let
in rec {
  firefox-bin = super.callPackage ./pkgs/networking/browsers/firefox-bin/darwin.nix {};
  hoverfly = super.callPackage ./pkgs/development/tools/hoverfly {};
  gopass = super.callPackage ./pkgs/tools/security/gopass {};
  pragmatapro = super.callPackage ./pkgs/data/fonts/pragmatapro {};
  chunkwm = super.callPackage ./pkgs/os-specific/darwin/chunkwm/default.nix {
    inherit (super.darwin.apple_sdk.frameworks) Carbon Cocoa ApplicationServices;
    imagemagick = super.imagemagick;
  };
  weechat = super.weechat.override {
    extraBuildInputs = [ super.python27Packages.websocket_client ];
  };
  emacs25Macport = super.stdenv.lib.overrideDerivation super.emacs25Macport (oldattrs: {
    emacsName = "emacs-25.2-rc1";
    name = "emacs-25.2-rc1-mac-6.2";
    src = super.fetchurl {
      url = "https://bitbucket.org/mituharu/emacs-mac/get/emacs-25.2-rc1-mac-6.2.tar.gz";
      sha256 = "c379201838677834158cff57d97b045d0e837662715c90009967905c3da83648";
    };
    hiresSrc = super.fetchurl {
      url = "https://s3.amazonaws.com/emacs-mac-port/Emacs.icns.modern";
      sha256 = "eb819de2380d3e473329a4a5813fa1b4912ec284146c94f28bd24fbb79f8b2c5";
    };
    postPatch = ''
      cp $hiresSrc mac/Emacs.app/Contents/Resources/Emacs.icns
    '';
    doCheck = false;
    postUnpack = null;
    macportSrc = null;
  });
  emacs = (if super.stdenv.isDarwin then emacs25Macport else super.emacs);
  inherit (super.callPackage ./pkgs/misc/uboot {})
    buildUBoot
    ubootTools
    ubootA20OlinuxinoLime
    ubootBananaPi
    ubootBeagleboneBlack
    ubootJetsonTK1
    ubootOdroidXU3
    ubootOrangePiPc
    ubootOrangePiPlusZero2
    ubootPcduino3Nano
    ubootRaspberryPi
    ubootRaspberryPi2
    ubootRaspberryPi3_32bit
    ubootRaspberryPi3_64bit
    ubootUtilite
    ubootWandboard
    ;
  # remacs = super.callPackage ./pkgs/applications/editors/emacs/remacs.nix {
  #   rust = super.latest.rustChannels.nightly.rust;
  #   inherit (darwin.apple_sdk.frameworks)
  #       AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit
  #       ImageCaptureCore GSS ImageIO;
  # };
}
