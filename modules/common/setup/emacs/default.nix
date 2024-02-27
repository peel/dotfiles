{ pkgs, ... }:

let
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
  '';
  myEmacs = pkgs.emacsGit.override{ withPgtk = pkgs.stdenv.isLinux; };
  myEmacsConfig = ./default.el;
in
pkgs.emacsWithPackagesFromUsePackage {
 config = builtins.readFile myEmacsConfig;
 package = myEmacs;
 override = epkgs: epkgs // {
   my-config = (pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
   '');
   eglot-booster = elisp (pkgs.fetchFromGitHub {
     owner = "jdtsmith";
     repo = "eglot-booster";
     rev = "e79dea640356eb4a8ed9df3808fe73c7c6db4cc4";
     sha256 = "sha256-ybNqMHCGjzT2+4OfywS7hNw551kIzwI3QqC8tU/GsQI=";
   }) "eglot-booster";
 };
 extraEmacsPackages = epkgs: with epkgs; [
   eglot-booster
   my-config
  ];
}
