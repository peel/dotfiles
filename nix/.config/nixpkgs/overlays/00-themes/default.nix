self: super:

let 
  theme = "dark";
in 
{
  colors = {} // (import ./colors.nix { });
  fonts = {} // import ./fonts.nix;
} 
