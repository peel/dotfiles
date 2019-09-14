self: super:

let 
  theme = "dark";
in 
{
  pragmatapro = super.callPackage ./pragmatapro {};
  colors = {} // (import ./colors.nix { });
  fonts = {} // import ./fonts.nix;
} 
