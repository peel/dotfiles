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
   chatgpt-shell = elisp (pkgs.fetchFromGitHub {
     owner = "xenodium";
     repo = "chatgpt-shell";
     rev = "b3a82b23d73263a00aef219f8055dcbe4fcecaec";
     sha256 = "sha256-RufzE5SUU1h+c4zoOTcaI/EFYw6favPF3nLgh0hEWiM=";
   }) "chatgpt-shell";
 };
 extraEmacsPackages = epkgs: with epkgs; [
    my-config
    chatgpt-shell
  ];
}
