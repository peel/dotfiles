let
    unstable = import <unstable> {};
    nixpkgs = import <nixpkgs> {};
in
with nixpkgs;
{
  nixpkgs.config.packageOverrides = pkgs : rec {
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
  };
  require = [
    ./setup/packages.nix
    ./setup/conf.nix
    ./setup/emacs.nix
    ./setup/osx.nix
  ];
}
