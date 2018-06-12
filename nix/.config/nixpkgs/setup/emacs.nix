{ pkgs, ... }:

let
  emacs = (import ../setup/emacs { inherit pkgs; });
in rec {
  environment.systemPackages = with pkgs; [
    fasd
    fzf
    global # gnu global for tagging code
    ripgrep
  ] ++ [ emacs ];
  services.emacs.enable = true;
}
