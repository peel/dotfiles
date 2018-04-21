{ pkgs ? import <nixpkgs> {} }:

let
  inherit (import ((pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "nix-config";
    rev = "0a33b5bdc8be5841100e27233cf371336ab20241";
    sha256 = "0h40k597zfl57zw1l5bdak7rv9bwkrgvypjgf47p1ag3kmpjrkdy";
    # date = 2018-03-24T13:13:51-07:00;
  }) + "/overlays/10-emacs.nix") pkgs pkgs) emacs26;
  myEmacsConfig = ./default.el;
  emacs = emacs26;
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
    '')

    company
    editorconfig
    flycheck
    ivy
    magit
    projectile
    use-package
    which-key
    
    # themes
    gotham-theme
  ]) ++ (with epkgs.melpaStablePackages; [
    counsel
  ]) ++ (with pkgs; [
     git
  ]))
