{ fetchgit, lib, pinfiles ? ./. }:

with lib;
with builtins;

let
  pin = file: let 
    json = f: filterAttrs (n: _: n != "date")(fromJSON (readFile f));
    in fetchgit (json file);
  pins = let
    files = readDir pinfiles;
    in filterAttrs (n: _: hasSuffix ".json" n) files;
in mapAttrs' (name: _: nameValuePair (removeSuffix ".json" name) (pin "${toString pinfiles}/${name}")) pins
