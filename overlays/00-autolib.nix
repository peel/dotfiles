self: pkgs:

with pkgs.lib;

{
  lib = pkgs.lib // {
    files = rec {

      /**
      * Lists paths of type `type` within `dir`
      * byType :: String -> Path -> [String]
      **/
      byType = type: dir: mapAttrsToList (n: _: n) (filterAttrs (n: v: v == type) (builtins.readDir dir));

      /**
      * Lists files within `dir`
      * regular :: Path -> [String]
      **/
      regular = byType "regular";

      /**
      * Lists files within `dir` with extension of `ext`
      * regularOf :: [String] -> Path -> [String]
      **/
      regularOf = ext: dir: builtins.filter (strings.hasSuffix ext) (regular dir);

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
      validPaths = paths: with builtins;
        let check = paths: foldl' (x: y: x && (pathExists y)) true paths;
        in check paths || trace "weechat-config: defined extra config files missing. Configuration will not be applied." false;
    };
  };
}

