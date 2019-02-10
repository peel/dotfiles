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
      cabal2nix
      ghcid
      hasktags
      hlint
      hindent
      structured-haskell-mode
      stylish-haskell
    ]);

  scalaEnv = self.buildEnv {
    name = "scala";
    paths = with self; [
      bloop
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

  awsEnv =
  let terraform = self.terraform.overrideAttrs(oldAttrs: rec {
  name = "terraform-${version}";
    version = "0.11.7";
    src = super.fetchFromGitHub {
      owner = "hashicorp";
      repo = "terraform";
      rev = "v${version}";
      sha256 = "0q5gl8yn1f8fas1v68lz081k88gbmlk7f2xqlwqmh01qpqjxd42q";
    };
  });
  in self.buildEnv {
    name = "aws";
    paths = with self; [
      awscli
      awslogs
    ] ++ [ terraform ];
  };

  nodeEnv = self.buildEnv {
    name = "node";
    paths = with self; [
      nodejs
      yarn
      nodePackages.prettier
    ];
  };
    
  airflowEnv = (self.python3.withPackages(ps: with ps; [
    nose
    mock
    numpy
    pandas
    xlrd
  ]));
}
