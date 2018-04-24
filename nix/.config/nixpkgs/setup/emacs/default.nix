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

    ace-window # window switcher
    #avy?
    #anzu?
    #aggresive-indent
    #clean-aindent-mode
    company
    #dash-at-point !
    #diff-hl?
    diminish
    #dumb-jump?
    editorconfig
    #exec-path-from-shell
    #eyebrowse?
    #fasd?
    flx # fuzzy matcher 
    flycheck
    #ggtags !!!
    #gist
    #git-gutter !!
    #git-gutter+
    #git-gutter-fringe
    #git-gutter-fringe+
    #git-link
    #hightlight
    highlight-symbol
    #highlight-parentheses
    #hl-todo
    #hydra !!!
    counsel
    #counsel-dash !
    counsel-projectile
    #all-the-icons
    #counsel-gtags !!!
    magit
    #magithub
    #pbcopy?
    #pdf-tools
    #popup?
    #popwin?
    #powerline
    projectile
    #rainbow-delimiters
    #rainbow-identifiers
    #rainbow-mode?
    #restart-emacs
    #smartparens !!!
    undo-tree
    use-package
    # with-editor
    which-key
    #yasnippet

    # themes
    gotham-theme

    #languages
    #fish-mode
    #graphviz-dot-mode
    #haskell
    #javascript
    #restclient !

    #scala
    # ensime 
    # scala-mode !!!
    # sbt-mode !!!

    #nix
    # nix-mode !!
    # company-nixos-options !
    # helm-nixos-options
    # nix-sandbox 
    # nix-buffer

    #org-mode !!
    #ivy-bibtex
    #biblio/biblio-core/interleave/org-ref/parsebib
    #ob-http
    #ob-restclient
    #org-plus-contrib
    #org-projectile
    #orgit
    #toc-org?

    #tools
    # pass
  ]) ++ (with epkgs.melpaStablePackages; [
  ]) ++ (with pkgs; [
     git
     ripgrep
  ]))
