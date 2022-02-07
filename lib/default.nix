self: pkgs:

with pkgs.lib;
with builtins;

{
  files = rec {

    /**
      * Lists paths of type `type` within `dir`
      * byType :: String -> Path -> [String]
      **/
    byType = type: dir: mapAttrsToList (n: _: n) (filterAttrs (n: v: v == type) (readDir dir));

    /**
      * Lists files within `dir`
      * regular :: Path -> [String]
      **/
    regular = byType "regular";

    /**
      * Lists files within `dir` with extension of `ext`
      * regularOf :: [String] -> Path -> [String]
      **/
    regularOf = ext: dir: filter (strings.hasSuffix ext) (regular dir);

    /**
      * Check if `dir` contains a regular *file* of type `ext`
      * contains :: String -> Path -> Bool
      **/
    contains = ext: dir: (lists.length (regularOf ext dir)) > 0;

    /**
      *
      * Checks whether `paths` exist on filesystem
      * validPaths :: [String] -> Bool
      **/
    validPaths = paths:
      let check = paths: foldl' (x: y: x && (pathExists y)) true paths;
      in check paths || trace "weechat-config: defined extra config files missing. Configuration will not be applied." false;
  };
  modules = rec {
    mapModules = path: fn: with lib; with builtins;
      let apply = fn: path: n: fn (path + ("/" + n));
          attrsIn = path: attrNames (readDir path);
          isModuleIn = path: n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix"));
          named = n: x: nameValuePair ((removeSuffix ".nix") n) x;
      in
        listToAttrs (map
          (n: named n (apply fn path n))
          (filter (isModuleIn path) (attrsIn path)));
  };
  stdenv.targetSystem = rec {
    isDarwinArm64 = pkgs.targetSystem.isDarwin && pkgs.targetSystem.darwinArch == "arm64";
  };
}
