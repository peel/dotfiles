{ pkgs ? import <nixpkgs> {} }:

let
  inherit (import ((pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "nix-config";
    rev = "0a33b5bdc8be5841100e27233cf371336ab20241";
    sha256 = "0h40k597zfl57zw1l5bdak7rv9bwkrgvypjgf47p1ag3kmpjrkdy";
    # date = 2018-03-24T13:13:51-07:00;
  }) + "/overlays/10-emacs.nix") pkgs pkgs) emacs26;
  prettifyPragmata = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/fabrizioschiavi/pragmatapro/master/emacs_snippets/pragmatapro-prettify-symbols-v0.826.el";
    sha256 = "1iy29y8k59dqwml0f1dadlxxhrp64q2r4k5xgm0kpmd4qw1frjgg";
  };
  oxJekyllSubtree = pkgs.fetchFromGitHub {
    owner = "Malabarba";
    repo = "ox-jekyll-subtree";
    rev = "d1da16e60b77f09bc2183ff1151e8965b3945527";
    sha256= "0ps4cz01y00w3913c4yxxmmlsg99wiqc6cnbpxs73h618xqfpq8b";
  };
  myEmacsConfig = ./default.el;
  emacs = emacs26;
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${prettifyPragmata} $out/share/emacs/site-lisp/pragmata.el
    cp ${oxJekyllSubtree}/*.el $out/share/emacs/site-lisp/
    cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
    '')

    # ace-window # window switcher
    avy
    #anzu?
    aggressive-indent
    #clean-aindent-mode
    company
    #dash-at-point !
    diff-hl
    diminish
    dumb-jump
    editorconfig
    expand-region
    #exec-path-from-shell
    #eyebrowse?
    #fasd?
    flx # fuzzy matcher 
    flycheck
    ggtags
    #gist
    #git-link
    #hightlight
    highlight-symbol
    #highlight-parentheses
    #hl-todo
    hydra
    counsel
    smex
    #counsel-dash !
    counsel-projectile
    #all-the-icons
    counsel-gtags
    magit
    #magithub !!
    noflet
    #popup?
    #popwin?
    projectile
    rainbow-delimiters
    rainbow-identifiers
    #rainbow-mode?
    restart-emacs
    # treemacs
    undo-tree
    use-package
    dired-sidebar
    exec-path-from-shell
    which-key
    #yasnippet

    # themes
    gotham-theme
    nord-theme
    apropospriate-theme

    # languages
    ## dhall
    dhall-mode
    #fish-mode
    #graphviz-dot-mode
    ## haskell
    ## javascript
    tide
    prettier-js
    rjsx-mode
    web-mode
    ## http
    restclient

    ## nix
    nix-mode
    # nix-shell
    # company-nixos-options !
    # helm-nixos-options
    # nix-sandbox 
    # nix-buffer

    ## scala
    ensime
    scala-mode
    sbt-mode
    
    ## markups
    dockerfile-mode
    markdown-mode
    terraform-mode
    yaml-mode

    ## org-mode !!
    org-ref
    interleave
    ivy-bibtex
    pdf-tools
    biblio
    biblio-core
    parsebib
    #ob-http
    #ob-restclient
    #org-plus-contrib
    #org-projectile
    #orgit
    #toc-org?

    #tools
    # pass
  ]) ++ (with epkgs.melpaStablePackages; [
    smartparens
  ]) ++ (with pkgs; [
    git
    global
    gnupg
    #pragmatapro
    ripgrep

    #scala
    #sbt
    #scalafmt
    #scalafix
    #mill
    
    #org
    graphviz
    
    #js
    # nodejs
    # nodePackages.prettier
    # nodePackages.eslint
    # nodePackages.eslint-plugin-react
    # nodePackages.eslint-plugin-jest
  ]))
