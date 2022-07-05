{
  inputs.nixpkgs.url = "nixpkgs/nixos-22.05";

  outputs = flakes @ { self, nixpkgs }: {
    nixosConfigurations.bastion = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ (import ./configuration.nix flakes) ];
    };

    devShell."aarch64-darwin" =
      with nixpkgs.legacyPackages.x86_64-linux;
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
