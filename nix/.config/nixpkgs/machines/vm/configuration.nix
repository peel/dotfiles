{ config, lib, pkgs, withGui ? false, ... }:

with lib;

let
  sources = import /dotfiles/setup/pinned { inherit (pkgs) fetchgit lib; };
  username = "peel";
  hostName = "vm";
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  nixpkgs.overlays = 
    let path = /dotfiles/overlays ; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
            (attrNames (readDir path)))
      ++ [ (import /nurpkgs/overlay.nix) ];
  nix.useSandbox = true;
  nix.binaryCaches = [ https://cache.nixos.org https://peel.cachix.org ];
  nix.trustedUsers = [ "${username}" "root" ];
  nix.nixPath = [
    "nixpkgs=${sources.nixpkgs}"
    # "nixos-config=/dotfiles/machines/${hostName}/configuration.nix"
    "nurpkgs-peel=/nurpkgs"
    "dotfiles=/dotfiles"
  ];
  
  imports = [
    ./vagrant.nix
    /dotfiles/setup/common.nix
    /dotfiles/setup/nixos.nix
    /dotfiles/setup/packages.nix
  ];


  # os ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
  system.stateVersion = "18.03";
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
  };

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  users.extraUsers = {
    "${username}"= {
      home = "/home/${username}";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1hNlicWZhVgkYg6DlSs1CkjbU710RFhIc9uUFdSlk3MLtjidI0Zfbn3bfqyerXwRzOuyL7/bEuqC9lvu7b6KmvEpj1nhuOr0U1ttgIzUvgmz/cwVcVdtLv9Cr21Y2yMRDkAjj3EyLgXXZ+2sii39pYlMHcf7KFCGwgvAlHiI8ThXIW+ZbGqU6NtyoxIBPSZSbZiUz4wz02azipwFGtmGKT37sMeL635YjWqMYMJD453vPdW2F2rpq4B1NZX5L/chZs0FRytjtJJWrm1IEaqUqZXvhktKC6pDLMm/2fuqLgU8DxltXBf5vqimsJ1LPMXGOgQ0OKExiF/Gkv1Ub9x80qcEfpXninz2cvGw0xWpr7FbEShkbalt5MN7kvLIzB4Kmw71L7Af4CxcyJ0cHGGjzg8B92k1gk1rItqM8e8mGeLMrEFPJxMUCJvIE7Ywgbu7kUM8WB5x+Ni4ZmeIUHAtUUad+Z6Xd1Y7lB7190HeWWUR9+yySxOH4pUA3161sCOa5xG/mnW7QtBbD4Tqnc7ouIUq57JpSvOtilTgQAQfS3NZkNbsOv2lHDltPCqk8F4039UCxps6EbJkgNmtV31jqTiDDdQ0fCmOcl8dNygA5cTkWgMjdf1U+va68s92fVsCrwQiKm3zFNGC6VHsSAkEMc29a4d70lWCZlrcWAglZdw== piotr@codearsonist.com"
      ];
    };
  };

  services = {
    openssh = {
      enable = true;
    };
    avahi = {
      enable = true;
      nssmdns = true;
      publish.addresses = true;
      publish.enable = true;
      publish.workstation = true;
      publish.domain = true;
    };
    xserver.enable = false;
    nixosManual.enable = false;
  };

}
