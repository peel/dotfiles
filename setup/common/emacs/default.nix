{ pkgs, ... }:

let
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
  '';
  myEmacs = (pkgs.emacsGcc.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
      pkgs.darwin.apple_sdk.frameworks.CoreFoundation
      pkgs.darwin.apple_sdk.frameworks.WebKit
    ];}
  )).override{ withXwidgets = true; };
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
   hide-comnt = elisp (pkgs.fetchFromGitHub {
     owner = "emacsmirror";
     repo = "hide-comnt";
     rev = "d1e94f5152f20b2dc7b0d42898c1db37e5be57a6";
     sha256 = "002i9f97sq3jfknrw2nim1bhvj7xz3icviw7iffqmpmww4g1hq9l";
   }) "hide-comnt";
   clockifuck = elisp (pkgs.fetchFromGitHub {
     owner = "bit4bit";
     repo = "clockifuck";
     rev = "a2143194d77053ffbb80fb13b7f49de509159b03";
     sha256 = "1ji0mwmqa3i9fd82rzn9lyr9i47n0lmf4mv3ahf6xlk9rdszr76g";
   }) "clockifuck";
 };
 extraEmacsPackages = epkgs: with epkgs; [
    my-config
    prettify-pragmata
    hide-comnt
    clockifuck
  ];
}
