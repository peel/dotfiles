self: super:
 rec {
  alacritty = super.callPackage ./pkgs/applications/misc/alacritty {};
  chunkwm = super.recurseIntoAttrs (super.callPackage ./pkgs/os-specific/darwin/chunkwm {
        inherit (super) callPackage stdenv fetchFromGitHub imagemagick;
        inherit (super.darwin.apple_sdk.frameworks) Carbon Cocoa ApplicationServices;
  });
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
  firefox-bin = super.callPackage ./pkgs/networking/browsers/firefox-bin/darwin.nix {};
  gopass = super.callPackage ./pkgs/tools/security/gopass {};
  hoverfly = super.callPackage ./pkgs/development/tools/hoverfly {};
  ix = super.callPackage ./pkgs/misc/ix {};
  mill = super.callPackage ./pkgs/development/tools/mill {};
  ngrok = super.ngrok.overrideAttrs (oldAttrs: rec {
    src = if super.stdenv.system == "x86_64-darwin" then super.fetchurl {
      url = "https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-darwin-amd64.tgz";
      sha256 = "0cnpgd56dd2c4qb105qlvb7r2x80p49pqm79n0wm0s4vwg4kq1k1";
    } else oldAttrs.src;
    installPhase = if super.stdenv.isDarwin then ''
        mkdir -p $out/bin
        cp ngrok $out/bin
      ''
      else oldAttrs.installPhase;
    meta.platforms = super.lib.platforms.unix;
  });
  pragmatapro = super.callPackage ./pkgs/data/fonts/pragmatapro {};
  qarma = super.callPackage ./pkgs/misc/qarma {
    inherit (super) stdenv fetchFromGitHub pkgconfig;
    inherit (super.qt5) qtbase qmake qttools qtmacextras qtx11extras;
  };
  rofi-emoji = super.callPackage ./pkgs/misc/rofi-emoji {};
  rofi-wifi-menu = super.callPackage ./pkgs/misc/rofi-wifi-menu {};
  scripts = super.callPackage ./pkgs/misc/scripts {
    pkgs=self;
    stdenv=self.stdenv;
  };
  tmux-prompt = super.callPackage ./pkgs/misc/tmux-prompt {};
  wee-slack = super.callPackage ./pkgs/networking/weechat/wee-slack.nix {};
  zenity = super.callPackage ./pkgs/misc/zenity {};
  remacs = super.callPackage ./pkgs/applications/editors/emacs/remacs.nix {};

}
