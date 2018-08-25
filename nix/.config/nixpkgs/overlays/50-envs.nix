self: super: {

   haskellEnv = (self.haskellPackages.override {
      overrides = hself: hsuper: with super.haskell.lib; with hsuper; {
        hasktags = dontCheck hasktags;
      };
   }).ghcWithHoogle(
    haskellPackages: with haskellPackages; [
      # libraries
      aeson
      attoparsec
      conduit
      hashable
      mtl
      microlens
      parsec
      
      # tools
      cabal-install
      hasktags
      hlint
      hindent
      structured-haskell-mode
      stylish-haskell
    ]);

  scalaEnv = self.buildEnv {
    name = "scala";
    paths = with self; [
      scalafmt
      sbt
    ];
  };

  beamEnv = self.buildEnv {
    name = "beam";
    paths = with self; [
      elixir_1_7
    ] ++ (with beamPackages; [
      
    ]);
  };
}
