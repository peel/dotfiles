{pkgs ? import <nixpkgs> {} }:

# emergency vm  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
# when everything else fails
# vagrant 

let
  hande-hoch = ''
    echo "UPDATING NIXBOX"
    vagrant plugin install &&\
    vagrant-nixos-plugin &&\
    vagrant up &&\ 
    vagrant package --vagrantfile Vagrantfile --output nixbox
  '';
  kaput = "vagrant destroy && vagrant box remove --all --force";
in pkgs.mkShell {
  buildInputs = [ pkgs.vagrant ];
  shellHook = ''
    alias hande-hoch=${hande-hoch}
    alias kaput=${kaput}
  '';
  
}
