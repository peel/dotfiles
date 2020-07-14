self: super: {

   haskellEnv = (self.haskellPackages.override {
      overrides = hself: hsuper: with super.haskell.lib; with hsuper; {
        hasktags = dontCheck hasktags;
      };
   }).ghcWithPackages(
    haskellPackages: with haskellPackages; [
      cabal-install
      ghcid
      hasktags
      hlint
      hindent
      structured-haskell-mode
      stylish-haskell
    ]);

  awsEnv = self.buildEnv {
    name = "aws";
    paths = with self; [
      awscli
      awslogs
      terraform
      kubectl
    ];
  };

}
