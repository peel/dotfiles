{
  description = "peel's env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-unstable, emacs-overlay, home-manager, sops-nix, ... }@inputs:
    let
      # FIXME nixpkgs.lib.extend
      myLib = (import ./lib {inherit (nixpkgs) lib targetSystem;});
      inherit (myLib) mapModules;
      inherit (nixpkgs.lib.strings) hasInfix;
      inherit (nixpkgs.lib) nixosSystem attrValues traceValSeqN ;
      inherit (darwin.lib) darwinSystem;
      mkSystem =
        { hostname
        , user ? "peel"
        , system ? "x86_64-linux"
        , extraModules ? []
        , homeModules ? import ./modules/common/setup/home.nix
        , ...}:
        let
          linuxOr = a: b: if (hasInfix "linux" system) then a else b;
          systemFn = linuxOr nixosSystem darwinSystem;
          overlayModules = [{ nixpkgs.overlays = [ emacs-overlay.overlay ] ++ (attrValues self.overlays); }];
          systemModules = traceValSeqN 3 (attrValues (linuxOr self.nixosModules self.darwinModules));
          # FIXME load with systemModules
          configModules = traceValSeqN 2 (linuxOr [ ./modules/nixos/setup ] [ ./modules/darwin/setup ]);
          homeManagerModules = linuxOr home-manager.nixosModules.home-manager home-manager.darwinModules.home-manager;
        in systemFn {
          inherit system;
          specialArgs = { inherit system inputs; };
          modules = [
            { networking.hostName = hostname; }
            (./machines/${hostname}/configuration.nix)
            homeManagerModules {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${user} = homeModules;
              home-manager.sharedModules = [ sops-nix.homeManagerModules.sops  ];
            }
          ] ++ overlayModules ++ systemModules ++ configModules ++ extraModules;
        };
      secrets = import ./secrets.nix;
    in {
      overlays = mapModules ./overlays import;
      nixosModules = (mapModules ./modules/nixos import) // (mapModules ./modules/common import);
      darwinModules = (mapModules ./modules/darwin import) // (mapModules ./modules/common import);

      # FIXME double naming
      nixosConfigurations = {
        nuke = mkSystem rec {
          hostname = "nuke";
          system = "x86_64-linux";
          extraModules = [
            ./modules/nixos/setup
            { peel = {
                hassio = {
                  enable = true;
                  zigbee2mqtt = nixpkgs-unstable.legacyPackages.${system}.zigbee2mqtt;
                  home-assistant = "2025.2.4";
                  music-assistant = {
                    enable = true;
                    media = "/mnt/music";
                  };
                  matter-server = {
                    enable = true;
                    version = "7.0.1";
                  };
                  scrypted.enable = true;
                };
                arr.enable = true;
                arr.downloads = true;
                budget.enable = true;
                media = {
                  enable = true;
                  plex = {
                    enable = true;
                    package = (import nixpkgs-unstable { #FIXME
                      inherit system;
                      config.allowUnfree = true;
                    }).plex;
                  };
                  jellyfin.enable = true;
                  music = {
                    enable = true;
                    navidromePackage = nixpkgs-unstable.legacyPackages.${system}.navidrome;
                    navidromeExtraSettings = secrets.navidromeExtraSettings;
                  };
                  books = {
                    enable = true;
                    package = nixpkgs-unstable.legacyPackages.${system}.audiobookshelf;
                  };
                };
              };
            }
          ];
        };
        wrkvm = mkSystem {
          hostname = "wrkvm";
          system = "x86_64-linux";
        };
        wrkvm64 = mkSystem {
          hostname = "wrkvm64";
          system = "aarch64-linux";
        };
      };

      darwinConfigurations = {
        snowflake = mkSystem {
          hostname = "snowflake";
          system = "x86_64-darwin";
        };
        snowberry = mkSystem {
          hostname = "snowberry";
          system = "aarch64-darwin";
        };
      };

      packages = {
        aarch64-linux = {
          vmwareImage =
            self.nixosConfigurations.wrkvm64.config.system.build.vmwareImage;
        };
        x86_64-linux = {
          vmwareImage =
            self.nixosConfigurations.wrkvm.config.system.build.vmwareImage;
        };
      };

      # FIXME shell so it supports all nixes
      # devShell = import ./shell.nix {pkgs;};
    };
}

