{ lib, pinfiles ? ./. }:

with lib;
with builtins;

let
  pin = file: let 
    json = f: fromJSON (readFile f);
    fetch = {owner, repo, rev, sha256, ...}: fetchTarball {
      inherit sha256;
      url = "https://github.com/${owner}/${repo}/tarball/${rev}";
    }; in fetch (json file);
  pins = let
    files = readDir pinfiles;
    in filterAttrs (n: _: hasSuffix ".json" n) files;
in mapAttrs' (name: _: nameValuePair (removeSuffix ".json" name) (pin "${toString <setup/pinned>}/${name}")) pins
