{ pkgs, ... }:

let
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
  '';
  myEmacs = pkgs.emacsGit; #.override{ withXwidgets = true; withGTK3 = true; };
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
   prettify-pragmata = elisp (pkgs.fetchFromGitHub {
     owner = "lumiknit";
     repo = "emacs-pragmatapro-ligatures";
     rev = "87bc656ace7b15aa088537b6bd369ee49e323dc1";
     sha256 = "19np1zfkdfcjjyvzpnn0p6kppfh6isrwxy8bqs0jd7bjh98n42jd";
   }) "prettify-pragmata";
 };
 extraEmacsPackages = epkgs: with epkgs; [
    my-config
    prettify-pragmata
  ];
}
