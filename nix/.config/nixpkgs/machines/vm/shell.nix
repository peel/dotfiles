{pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [ vagrant ];
  shellHook = ''
    vagrant plugin install vagrant-nixos-plugin || true
  '';
}
