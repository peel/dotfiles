{ pkgs, ... }:

let
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
  '';
  myEmacs = pkgs.emacs-git.override{ withPgtk = pkgs.stdenv.isLinux; };
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
   readwise = elisp (pkgs.fetchFromGitHub {
     owner = "wdavew";
     repo = "readwise-el";
     rev = "5db1374a5c9129be35f8466d53d103fb520c20c6";
     sha256 = "sha256-8oe/hVh0+fLyupBYaqxT7H8RHLlkN1l+7+CJIBYqFvA=";
   }) "readwise";
   claude-code-ide = elisp (pkgs.fetchFromGitHub {
     owner = "manzaltu";
     repo = "claude-code-ide.el";
     rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
     sha256 = "sha256-nRe3cCF3EtK8zyuqwguzUotDH/cqoNedIqEM5HXjC/4=";
   }) "claude-code-ide";
   term-keys = elisp (pkgs.fetchFromGitHub {
     owner = "CyberShadow";
     repo = "term-keys";
     rev = "5677d06daad83bdf355ad2746c4582475cd0d3f8";
     sha256 = "sha256-H6CISfw3e0zEXBjj3UC1vJ6RekVF4IWH1Xis0NezPsE=";
   }) "term-keys";
 };
 extraEmacsPackages = epkgs: with epkgs; [
   eglot-booster
   readwise
   claude-code-ide
   auth-source-1password
   term-keys
   my-config
  ];
}
