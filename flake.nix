{
  description = "peel's env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/release-21.11";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-unstable, emacs-overlay, home-manager, ... }@inputs:
    let
      # FIXME move to lib and nixpkgs.lib.extend
      mapModules =
        path: fn: with nixpkgs.lib; with builtins;
          let apply = fn: path: n: fn (path + ("/" + n));
              attrsIn = path: attrNames (readDir path);
              isModuleIn = path: n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix"));
              named = n: x: nameValuePair ((removeSuffix ".nix") n) x;
          in
            listToAttrs (map
              (n: named n (apply fn path n))
              (filter (isModuleIn path) (attrsIn path)));
      mkSystem =
        { hostname
        , user ? "peel"
        , system ? "x86_64-linux"
        , extraModules ? []
        , homeModules ? import ./modules/common/setup/home.nix
        , ...}: with nixpkgs.lib; with builtins;
          let
            linuxOr = a: b: if (hasInfix "linux" system) then a else b;
            systemFn = linuxOr nixosSystem darwin.lib.darwinSystem;
            overlayModules = [{ nixpkgs.overlays = [ emacs-overlay.overlay ] ++ (attrValues self.overlays); }];
            systemModules = attrValues (linuxOr self.nixosModules self.darwinModules);
            # FIXME load with systemModules
            configModules = linuxOr [ ./modules/nixos/setup ] [ ./modules/darwin/setup ];
            homeManagerModules = linuxOr home-manager.nixosModules.home-manager home-manager.darwinModules.home-manager;
          in systemFn {
            inherit system;
            modules = [
              { networking.hostName = hostname; }
              ./machines/${hostname}/configuration.nix
              homeManagerModules {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${user} = homeModules;
              }
              # ./setup/common
            ] ++ overlayModules ++ systemModules ++ configModules ++ extraModules;
          };
    in {
      overlays = mapModules ./overlays import;
      nixosModules = (mapModules ./modules/nixos import) // (mapModules ./modules/common import);
      darwinModules = (mapModules ./modules/darwin import) // (mapModules ./modules/common import);

      nixosConfigurations = {
        nuke = mkSystem {
          hostname = "nuke";
          system = "x86_64-linux";
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

