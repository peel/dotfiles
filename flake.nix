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

  outputs = { self, darwin, nixpkgs, nixpkgs-unstable, emacs-overlay, flake-utils, home-manager, ... }@inputs:
    let
      # FIXME move to lib and nixpkgs.lib.extend
      mapModules = path: fn: with nixpkgs.lib; with builtins;
        let apply = fn: path: n: fn (path + ("/" + n));
            attrsIn = path: attrNames (readDir path);
            isModuleIn = path: n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix"));
            named = n: x: nameValuePair ((removeSuffix ".nix") n) x;
        in
          listToAttrs (map
            (n: named n (apply fn path n))
            (filter (isModuleIn path) (attrsIn path)));
      pkgs = import nixpkgs {
        allowUnfree = true;
        overlays = (nixpkgs.lib.attrValues self.overlays);
      };
    in {
      overlays = mapModules ./overlays import;
      # FIXME abstract
      darwinConfigurations.snowflake = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          { nixpkgs.overlays = [ emacs-overlay.overlay ]; }
          ./machines/snowflake/configuration.nix
        ];
      };
      darwinConfigurations.snowberry = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          { nixpkgs.overlays = [ emacs-overlay.overlay ]; }
          ./machines/snowberry/configuration.nix
        ] ++ (nixpkgs.lib.attrValues  self.darwinModules);
      };
      # FIXME
      nixosConfigurations.nuke = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/nuke/configuration.nix
          ./machines/nuke/hardware-configuration.nix
        ];
      };
      nixosConfigurations.wrkvm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ emacs-overlay.overlay ]; }
          ./machines/peel-work-vm/configuration.nix
          ./machines/peel-work-vm/hardware-configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.peel = import ./setup/common/home.nix;
          }
        ];
      };
      nixosConfigurations.wrkvm64 = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          # FIXME overlays
          { nixpkgs.overlays = [ emacs-overlay.overlay ] ++ (nixpkgs.lib.attrValues self.overlays); }
          ./machines/wrkvm64/configuration.nix
          ./machines/wrkvm64/hardware-configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.peel = import ./setup/common/home.nix;
          }
        ] ++ (nixpkgs.lib.attrValues self.nixosModules);
      };

      nixosModules = (mapModules ./modules/nixos import) // (mapModules ./modules/common import);
      darwinModules = (mapModules ./modules/darwin import) // (mapModules ./modules/common import);

      # FIXME
      packages.aarch64-linux = {
        vmwareImage =
          self.nixosConfigurations.wrkvm64.config.system.build.vmwareImage;
      };
      packages.x86_64-linux = {
        vmwareImage =
          self.nixosConfigurations.wrkvm.config.system.build.vmwareImage;
      };
      # FIXME shell so it supports all nixes
      # devShell = import ./shell.nix {inherit pkgs;};
    };
}
