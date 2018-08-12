{ pkgs ? import <nixpkgs> {} }:

let
  prettifyPragmata = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/fabrizioschiavi/pragmatapro/master/emacs_snippets/pragmatapro-prettify-symbols-v0.826.el";
    sha256 = "1iy29y8k59dqwml0f1dadlxxhrp64q2r4k5xgm0kpmd4qw1frjgg";
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
  myEmacs = pkgs.emacs;
  myEmacsConfig = ./default.el;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
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
    lsp-mode
    lsp-ui
    company-lsp
    ## dhall
    dhall-mode
    #fish-mode
    #graphviz-dot-mode
    ## haskell
    lsp-haskell
    structured-haskell-mode
    hindent
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
    #toc-org?

    #tools
    # pass
  ]) ++ (with epkgs.melpaStablePackages; [
    smartparens
  ]) ++ (with epkgs.elpaPackages; [
    undo-tree
  ]))
