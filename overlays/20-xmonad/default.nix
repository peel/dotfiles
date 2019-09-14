self: pkgs:

{
  peelxmonad = pkgs.haskellPackages.callPackage ./xmonad { inherit pkgs; };
  xmobar = import ./xmobar { inherit pkgs; };
}
