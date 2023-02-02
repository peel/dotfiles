{
  inputs.nixpkgs.url = "nixpkgs/nixos-22.11";

  outputs = flakes @ { self, nixpkgs }: {
    nixosConfigurations.bastion = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [ (import ./configuration.nix flakes) ];
    };

    devShell."aarch64-darwin" =
      with nixpkgs.legacyPackages.aarch64-darwin;
      mkShell {
        nativeBuildInputs = [
          awscli
	  jq
          (terraform.withPlugins (p: with p; [ aws p.null external ]))
        ];

        shellHook = ''
          alias tf=terraform
        '';
      };
  };
}
