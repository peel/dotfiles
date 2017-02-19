{ pkgs }:

rec {
  allowBroken = true;
  allowUnfree = true;
  allowUnfreeRedistributable = true;

  packageOverrides = pkgs : rec {
    scala-env = with pkgs; buildEnv {
      name = "scala-env";
      paths = [
            openjdk
            sbt
            scalafmt
      ];
    };
    erlang-env = with pkgs; buildEnv {
      name = "elrang-env";
      paths = [
            erlang
            elixir
      ];
    };
  };

}
