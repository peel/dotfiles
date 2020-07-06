with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (hs: [hs.turtle]))
  ];
}
