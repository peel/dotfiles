let
    unstable = import <unstable> {};
    nixpkgs = import <nixpkgs> {};
in
with nixpkgs;
{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nix.package = nixpkgs.nixUnstable;
  nix.nixPath =
    [
      "darwin=$HOME/.nix-defexpr/darwin"
      "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

  nixpkgs.config.packageOverrides = pkgs : rec {
    firefox-bin = pkgs.callPackage ./pkgs/networking/browsers/firefox-bin/darwin.nix {};
    hoverfly = pkgs.callPackage ./pkgs/development/tools/hoverfly {};
    chunkwm = pkgs.callPackage ./pkgs/os-specific/darwin/chunkwm/default.nix {
        inherit (pkgs.darwin.apple_sdk.frameworks) Carbon Cocoa ApplicationServices;
    };
    weechat = pkgs.weechat.override {
      extraBuildInputs = [ pkgs.python27Packages.websocket_client ];
    };
    emacs25Macport = pkgs.stdenv.lib.overrideDerivation pkgs.emacs25Macport (oldattrs: {
      emacsName = "emacs-25.2-rc1";
      name = "emacs-25.2-rc1-mac-6.2";
      src = fetchurl {
        url = "https://bitbucket.org/mituharu/emacs-mac/get/emacs-25.2-rc1-mac-6.2.tar.gz";
        sha256 = "c379201838677834158cff57d97b045d0e837662715c90009967905c3da83648";
      };
      hiresSrc = fetchurl {
        url = "https://s3.amazonaws.com/emacs-mac-port/Emacs.icns.modern";
        sha256 = "eb819de2380d3e473329a4a5813fa1b4912ec284146c94f28bd24fbb79f8b2c5";
      };
      postPatch = ''
        cp $hiresSrc mac/Emacs.app/Contents/Resources/Emacs.icns
      '';
      doCheck=false;
      postUnpack = null;
      macportSrc = null;
    });
    emacs = (if pkgs.stdenv.isDarwin then emacs25Macport else pkgs.emacs);
    rustNightly = callPackage ( fetchFromGitHub {
      owner = "solson";
      repo = "rust-nightly-nix";
      rev = "878a8aea8866c51b7f6b4d212536785c1034973f";
      sha256 = "05isbx0fvvj1pqx7k4d3jffy2xnp7wivhv6qdcv7k134axjrdp64";
    }){};
    remacs = pkgs.callPackage ./pkgs/applications/editors/emacs/remacs.nix {
      rust = rustNightly.rust { date = "2017-06-29"; hash="0axqij16dir0mj5k84hlka3rdm1ri5d2k6s94xbpcysd0vq1j2ag"; };
      inherit (darwin.apple_sdk.frameworks)
          AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit
          ImageCaptureCore GSS ImageIO;
    };
  };
  require = [
    ./setup/common.nix
    ./setup/darwin.nix
    ./setup/packages.nix
    ./setup/emacs.nix
    ./setup/osx.nix
  ];
}
