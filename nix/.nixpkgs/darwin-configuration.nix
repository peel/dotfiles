let
    unstable = import <unstable> {};
    nixpkgs = import <nixpkgs> {};
in
with nixpkgs;
{
  nixpkgs.config.packageOverrides = pkgs : rec {
    emacs25Macport = pkgs.stdenv.lib.overrideDerivation pkgs.emacs25Macport (oldattrs: {
      src = fetchurl {
        url = "https://bitbucket.org/mituharu/emacs-mac/get/emacs-25.1-mac-6.1.tar.gz";
        sha256 = "3ede57b06a20b361d5ed66d040e3b0adb1a84cbccabb6eaf21e2d8e8398de3b6";
      };
      hiresSrc = fetchurl {
        url = "https://s3.amazonaws.com/emacs-mac-port/Emacs.icns.modern";
        sha256 = "eb819de2380d3e473329a4a5813fa1b4912ec284146c94f28bd24fbb79f8b2c5";
      };
      postPatch = ''
        cp $hiresSrc mac/Emacs.app/Contents/Resources/Emacs.icns
      '';
      postUnpack = null;
      macportSrc = null;
    });
    emacs = (if pkgs.stdenv.isDarwin then emacs25Macport else pkgs.emacs);
  };
  require = [
    ./setup/packages.nix
    ./setup/conf.nix
    ./setup/emacs.nix
    ./setup/osx.nix
  ];
}