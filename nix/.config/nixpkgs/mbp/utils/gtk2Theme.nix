# Paramaters
# theme = {
#   package = the package name of the theme
#   name = The location of the theme in .../share/themes/
# }
#
# icons = {
#  package = the package name of the icon theme
#  name = The locaion of the icon pack in  .../share/icons/
# }
#
# returns a nix configuration function, so it 
# should be placed inside the imports configuration option.
# 
# usage example:
# in yout configuration.nix:
#
# imports = [ ... 
#   import .../.../gtk2Theme.nix { 
#      theme = {
#        package = pkgs.arc-theme;
#        name = "Arc";
#      };
#      icons = {
#        package = pkgs.breeze-icons; 
#        name = "Breeze";
#      }; 
#    }
#
# ... ]

{ theme, icons }:

{ pkgs, ...}:
let themeEnv =
  ''
    export GTK2_RC_FILES=${pkgs.writeText "iconrc" ''gtk-icon-theme-name="${icons.name}"''}:${theme.package}/share/themes/${theme.name}/gtk-2.0/gtkrc:$GTK2_RC_FILES
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
  '';
in
{
  environment.extraInit = themeEnv;
  environment.systemPackages = [ 
    theme.package 
    icons.package 
  ];
}
