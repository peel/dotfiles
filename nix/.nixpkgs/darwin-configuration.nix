{
  nixpkgs.config.packageOverrides = pkgs : rec {
    emacs = (if pkgs.stdenv.isDarwin then pkgs.emacs25Macport else pkgs.emacs);
  };
  require = [
    ./setup/packages.nix
    ./setup/conf.nix
    ./setup/emacs.nix
    ./setup/osx.nix
  ];
}
