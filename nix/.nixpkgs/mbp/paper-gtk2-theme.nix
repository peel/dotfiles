pkgs:
let
  gtk2theme = {
    package = pkgs.paper-gtk-theme;
    name = "Paper";
  };

  iconTheme = {
    package = pkgs.paper-icon-theme;
    name = "Paper";
  };

in
import ./utils/gtk2Theme.nix {
  theme = gtk2theme;
  icons = iconTheme;
}
