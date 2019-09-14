let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.my-package.components.all
