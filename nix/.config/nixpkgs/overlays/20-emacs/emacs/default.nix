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
  structured-haskell-mode = pkgs.emacsPackagesNg.melpaBuild {
    pname = "shm";
    version = "20170523";
    src = pkgs.fetchFromGitHub {
      owner = "chrisdone";
      repo = "structured-haskell-mode";
      rev = "7f9df73f45d107017c18ce4835bbc190dfe6782e";
      #date: 2018-03-26T20:57:49-04:00;
      sha256 = "1jcc30048j369jgsbbmkb63whs4wb37bq21jrm3r6ry22izndsqa";
    };
    packageRequires = [pkgs.emacsPackagesNg.haskell-mode ];
    fileSpecs = [ "elisp/*.el" ];
    propagatedUserEnvPkgs = [pkgs.haskellPackages.structured-haskell-mode ];
    recipe = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/milkypostman/melpa/68a2fddb7e000487f022b3827a7de9808ae73e2a/recipes/shm";
      sha256 = "1qmp8cc83dcz25xbyqd4987i0d8ywvh16wq2wfs4km3ia8a2vi3c";
      name = "recipe";
    };
    meta = {
      description = "Structured editing Emacs mode for Haskell";
      license = pkgs.lib.licenses.bsd3;
      platforms = pkgs.haskellPackages.structured-haskell-mode.meta.platforms;
    };
  };
  overrides = self: super: rec {
  emacs-libvterm = super.emacs-libvterm.overrideAttrs(attrs: rec {
      version = "unstable-2018-10-25";
      name = "emacs-libvterm-${version}";
      src = pkgs.fetchFromGitHub {
        owner = "akermu";
        repo = "emacs-libvterm";
        rev = "9738bb97f5a269b1a68a9e785236eb2b36a37562";
        #"date": "2018-10-25T22:45:10+02:00",
        sha256 = "1rmsn9isd990838hva48z8yf6bwqs3gn3594n0p4skf8k4ll6ki0";
      };
    });
  };
  myEmacs = pkgs.emacs;
  myEmacsConfig = ./default.el;
  emacsWithPackages = ((pkgs.emacsPackagesNgGen myEmacs).overrideScope' overrides).emacsWithPackages;
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
    editorconfig
    expand-region
    xterm-color
    shell-pop
    esh-autosuggest
    eshell-prompt-extras
    #eyebrowse?
    #fasd?
    flx # fuzzy matcher 
    flycheck
    ggtags
    #gist
    #git-link
    #hightlight
    highlight-symbol
    highlight-stages
    #highlight-parentheses
    #hl-todo
    hydra
    counsel
    smex
    #counsel-dash !
    counsel-projectile
    counsel-gtags
    magit
    git-link
    # magithub
    #popup?
    #popwin?
    projectile
    rainbow-delimiters
    rainbow-identifiers
    #rainbow-mode?
    # restart-emacs
    # treemacs
    # undo-tree
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
    # lsp-mode
    # lsp-ui
    # company-lsp

    ## dhall
    dhall-mode
    #graphviz-dot-mode

    ## elixir
    elixir-mode
    alchemist

    ## haskell
    # lsp-haskell
    structured-haskell-mode
    hindent
    dante
    attrap
    
    ## javascript
    # tide
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
    direnv
    nix-buffer

    ## scala
    scala-mode
    sbt-mode
    
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
  ]) ++ (with epkgs.elpaPackages; [
    undo-tree
  ]) ++ (with epkgs; [
    emacs-libvterm
  ]))
