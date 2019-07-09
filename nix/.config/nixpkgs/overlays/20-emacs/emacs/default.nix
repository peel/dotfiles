{ pkgs ? import <nixpkgs> {} }:

let
  prettifyPragmata = pkgs.fetchFromGitHub {
    owner = "DeLaGuardo";
    repo = "pragmata-pro.el";
    rev = "90e1ac87d10820ea345739a08e942acd05c5d6d7";
    # date = "2017-09-28T11:17:44+02:00";
    sha256 = "1x2agjfpbkh0ni8yp9asdfi3i3favpyz4jzxjhjp7cxndn3fcr66";
  };
  oxJekyllSubtree = pkgs.fetchFromGitHub {
    owner = "Malabarba";
    repo = "ox-jekyll-subtree";
    rev = "d1da16e60b77f09bc2183ff1151e8965b3945527";
    sha256 = "0ps4cz01y00w3913c4yxxmmlsg99wiqc6cnbpxs73h618xqfpq8b";
  };
  libvterm-neovim = pkgs.libvterm-neovim.overrideAttrs(attrs: rec {
    src = pkgs.fetchFromGitHub {
     owner = "neovim";
     repo = "libvterm";
     rev = "89675ffdda615ffc3f29d1c47a933f4f44183364";
     sha256 = "0l9ixbj516vl41v78fi302ws655xawl7s94gmx1kb3fmfgamqisy";
   };
  });
  overrides = self: super: rec {  };
  myEmacs = pkgs.emacs;
  myEmacsConfig = ./default.el;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${prettifyPragmata}/* $out/share/emacs/site-lisp/
    cp ${oxJekyllSubtree}/*.el $out/share/emacs/site-lisp/
    cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
    '')

    # ace-window # window switcher
    # avy
    #anzu?
    #clean-aindent-mode
    company
    dash-at-point
    diff-hl
    diminish
    dumb-jump
    expand-region
    xterm-color
    #eyebrowse?
    flx
    flycheck
    #hightlight
    highlight-stages
    #highlight-parentheses
    #hl-todo
    hydra
    counsel
    smex
    #counsel-dash !
    counsel-projectile
    
    # git
    magit
    forge
    git-link
    gitignore-mode
    
    projectile
    rainbow-delimiters
    rainbow-identifiers
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
    lsp-mode
    lsp-ui
    company-lsp

    ## dhall
    dhall-mode
    #graphviz-dot-mode

    ## elixir
    elixir-mode
    alchemist

    ## haskell
    haskell-mode
    hindent
    dante
    attrap
    
    ## javascript
    prettier-js
    rjsx-mode
    web-mode
    
    ## http
    restclient

    ## nix
    nix-mode
    direnv
    nix-buffer
    nix-sandbox

    ## scala
    scala-mode
    sbt-mode
    lsp-scala
    
    ## markups
    dockerfile-mode
    markdown-mode
    terraform-mode
    yaml-mode

    ## org-mode
    ob-async
    org-ref
    org-noter
    ivy-bibtex
    pdf-tools
    biblio
    biblio-core
    parsebib
    nov
    #ob-http
    #ob-restclient
    #org-plus-contrib
    #org-projectile
    #orgit
  ]) ++ (with epkgs.melpaStablePackages; [
    smartparens
  ]) ++ (with epkgs; [
    emacs-libvterm
    structured-haskell-mode
  ]))
