{
  description = "peel's env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";    
  };

  outputs = { self, darwin, nixpkgs, emacs-overlay, home-manager, ... }@inputs: {
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
      ];
    };
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
        { nixpkgs.overlays = [ emacs-overlay.overlay ]; }
        ./machines/wrkvm64/configuration.nix
        ./machines/wrkvm64/hardware-configuration.nix
        home-manager.nixosModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.peel = import ./setup/common/home.nix;
        }
      ];
    };
    packages.aarch64-linux = {
       vmwareImage =
         self.nixosConfigurations.wrkvm64.config.system.build.vmwareImage;
     };
    packages.x86_64-linux = {
       vmwareImage.x86_64-linux =
         self.nixosConfigurations.wrkvm.config.system.build.vmwareImage;
       vmwareImage.aarch64-linux = 
	       self.nixosConfigurations.wrkvm64.config.system.build.vmwareImage;
    };
  };
 }
