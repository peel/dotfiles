{ sources ? ../../pinned/sources.nix
, pkgs ? sources.nixpkgs
, config
, lib
, ...
}:

(import ../../setup/darwin { inherit config pkgs; }) // {
  peel.setup.host = "snowflake";
}
