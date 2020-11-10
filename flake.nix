{
  description = "peel's env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, emacs-overlay, ... }@inputs: {
    darwinConfigurations."snowflake" = darwin.lib.darwinSystem {
      modules = [
        { nixpkgs.overlays = [ emacs-overlay.overlay ]; }
        ./machines/snowflake/configuration.nix
        # home-manager.nixosModules.home-manager
        #   {
        #     home-manager.useGlobalPkgs = true;
        #     home-manager.useUserPackages = true;
        #     home-manager.users.user = import ./home.nix;
        #   }        
      ];
      
    };
  };
}
