self: pkgs:

{
  emacs = import ./emacs { inherit pkgs; };
}
