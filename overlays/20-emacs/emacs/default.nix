{ pkgs ? import <nixpkgs> { overlays = [(import (builtins.fetchTarball {
      url = https://github.com/peel/emacs-overlay/archive/master.tar.gz;
    }))];} }:

let
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
  '';
  myEmacs = pkgs.emacsGit.overrideAttrs(old: {
    patches = old.patches ++ [];
  });
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
 };
 extraEmacsPackages = epkgs: with epkgs; [
    # avy
    company
    diff-hl
    diminish
    dumb-jump
    expand-region
    flx
    flycheck
    #hightlight
    highlight-stages
    #highlight-parentheses
    #hl-todo
    hydra
    counsel
    request # counsel-search
    smex
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
    writeroom-mode
    focus
    yasnippet
    yasnippet-snippets
    smartparens
    dash-at-point
    vterm
    
    # themes
    gotham-theme
    apropospriate-theme

    # lsp
    lsp-mode
    company-lsp
    lsp-ui
    lsp-ivy
    
    ## dhall
    dhall-mode

    ## elixir
    elixir-mode
    alchemist

    ## haskell
    haskell-mode
    ormolu
    dante
    attrap
    structured-haskell-mode
    
    ## javascript
    prettier-js
    js2-mode
    typescript-mode
    web-mode
    
    ## http
    restclient

    ## rust
    rust-mode
    
    ## go
    go-mode

    ## nix
    nix-mode
    envrc
    nix-buffer
    nix-sandbox

    ## scala
    scala-mode
    lsp-metals
    
    ## markups
    dockerfile-mode
    markdown-mode
    terraform-mode
    yaml-mode

    ## org-mode
    org-roam
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
    #graphviz-dot-mode
  ] ++
  [
    my-config
    prettify-pragmata
    hide-comnt
  ];
}
