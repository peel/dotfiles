self: super:
 rec {
  alacritty = super.callPackage ./pkgs/applications/misc/alacritty {};
  chunkwm = super.recurseIntoAttrs (super.callPackage ./pkgs/os-specific/darwin/chunkwm {
        inherit (super) callPackage stdenv fetchFromGitHub imagemagick;
        inherit (super.darwin.apple_sdk.frameworks) Carbon Cocoa ApplicationServices;
  });
  skhd = super.skhd.overrideAttrs(oldAttrs: rec {
    name = "skhd-${version}";
    version = "0.2.2";
    src = super.fetchFromGitHub {
      owner = "koekeishiya";
      repo = "skhd";
      rev = "v${version}";
      sha256 = "0mn6svz2mqbpwlx510r447vflfcxryykpin6h6429dlz0wjlipa8";
    };
  });
  emacsPlus = let
    patchMulticolorFonts = super.fetchurl {
        url = "https://gist.githubusercontent.com/aatxe/260261daf70865fbf1749095de9172c5/raw/214b50c62450be1cbee9f11cecba846dd66c7d06/patch-multicolor-font.diff";
        sha256 = "5af2587e986db70999d1a791fca58df027ccbabd75f45e4a2af1602c75511a8c";
    };
    # needs 10.11 sdk
    patchBorderless = super.fetchurl {
        url = "https://raw.githubusercontent.com/peel/GNU-Emacs-OS-X-no-title-bar/master/GNU-Emacs-OS-X-no-title-bar.patch";
        sha256 = "0cjmc0nzx0smc4cxmxcjy75xf83smah3fkjfyql1y14gd59c1npw";
    };
    patchPixelScrolling = super.fetchurl {
        url = "https://gist.githubusercontent.com/aatxe/ecd14e3e4636524915eab2c976650576/raw/c20527ab724ddbeb14db8cc01324410a5a722b18/emacs-pixel-scrolling.patch";
        sha256 = "34654d889e8a02aedc0c39a0f710b3cc17d5d4201eb9cb357ecca6ed1ec24684";
    };
    patch24bitColor = super.fetchurl {
        url = "https://gist.githubusercontent.com/akorobov/2c9f5796c661304b4d8aa64c89d2cd00/raw/2f7d3ae544440b7e2d3a13dd126b491bccee9dbf/emacs-25.2-term-24bit-colors.diff";
        sha256 = "ffe72c57117a6dca10b675cbe3701308683d24b62611048d2e7f80f419820cd0";
    };
  in {
      with24bitColor ? false
    , withPixelScrolling ? false
    , withBorderless ? false
    , withMulticolorFonts ? false
  }: (super.emacs
      .override{srcRepo=true;inherit (super) autoconf automake texinfo;})
      .overrideAttrs (oldAttrs: rec {
        patches = oldAttrs.patches
          ++ super.lib.optional with24bitColor patch24bitColor
          ++ super.lib.optional withPixelScrolling patchPixelScrolling
          ++ super.lib.optional withBorderless patchBorderless
          ++ super.lib.optional withMulticolorFonts patchMulticolorFonts;
      });
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
  # remacs = super.callPackage ./pkgs/applications/editors/emacs/remacs.nix {};
  rofi-emoji = super.callPackage ./pkgs/misc/rofi-emoji {};
  rofi-wifi-menu = super.callPackage ./pkgs/misc/rofi-wifi-menu {};
  scripts = super.callPackage ./pkgs/misc/scripts {
    pkgs=self;
    stdenv=self.stdenv;
  };
  tmux-prompt = super.callPackage ./pkgs/misc/tmux-prompt {};
  wee-slack = super.callPackage ./pkgs/networking/weechat/wee-slack.nix {};
  zenity = super.callPackage ./pkgs/misc/zenity {};
}
