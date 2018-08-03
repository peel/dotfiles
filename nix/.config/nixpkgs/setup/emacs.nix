{ pkgs, ... }:

let
  emacs = (import ../setup/emacs { inherit pkgs; });
in rec {
  environment.systemPackages = [ emacs ];
  services.emacs.enable = true;
  services.emacs.package = emacs;
}
