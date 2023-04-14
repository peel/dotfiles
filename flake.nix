{
  description = "peel's env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    depot-tools.url = "github:cir0x/depot-tools-nix-flake";
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-unstable, emacs-overlay, home-manager, depot-tools, ... }@inputs:
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
            modules = [
              { networking.hostName = hostname; }
              (./machines/${hostname}/configuration.nix)
              homeManagerModules {
               home-manager.useGlobalPkgs = true;
               home-manager.useUserPackages = true;
               home-manager.users.${user} = homeModules;
              }
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
          extraModules = [ ./modules/nixos/setup ./modules/common/setup/hassio.nix ];
        };
        demo = mkSystem {
          hostname = "demo";
          system = "aarch64-linux";
          extraModules = [
            ./modules/nixos/setup
            {
              virtualisation.host.pkgs = nixpkgs.legacyPackages.aarch64-darwin;
              nixpkgs.pkgs = nixpkgs.legacyPackages.aarch64-linux.pkgs;
              virtualisation.sharedDirectories = {
                wrk = {
                  source = "/Users/peel/wrk";
                  target = "/home/peel/wrk";
                };
              };
            }
          ];
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
        aarch64-darwin = let pkgs = nixpkgs.legacyPackages.aarch64-darwin.pkgs; in rec {
          libangle = let
            git_url = "https://chromium.googlesource.com";
            deps = {
              "build" = pkgs.fetchgit {
                url    = "${git_url}/chromium/src/build.git";
                rev    = "71ce49253a9af268a035cbe58e28b4dd7b36a9f5";
                sha256 = "OF0pjGkW/aoFu9c2Pt5gEAcKRLP5j8xE8at4AjtMeX0=";
              };
              "config" = pkgs.fetchgit {
                url    = "${git_url}/chromium/src/build/config.git";
                rev    = "aca8b4d46bec59a930146fe6057ef297cb297bec";
                sha256 = "mJzz8WrWdQ3FTjN8GmzrlfJliZmV56EoJ0PcDar9KA4=";
              };
            };
            in pkgs.stdenv.mkDerivation {
            name = "libangle";
            vesion = "2023-04-07";
            src = pkgs.fetchgit {
              url = "https://chromium.googlesource.com/angle/angle";
              rev = "23ec06204174174a3bb5bec3edad6df87bbd50b2";
              sha256 = "sha256-Qa7fA6hxaTFrWptncp6ximWMKVlCXCd/ifxbBkihMB8=";
            };
            nativeBuildInputs = [
              depot-tools.packages.aarch64-darwin.gclient
            ];
            buildInputs = [
              pkgs.meson
              pkgs.gn
              pkgs.ninja
              pkgs.gcc
            ];
            postUnpack = ''
              ${pkgs.lib.concatStringsSep "\n" (
                  pkgs.lib.mapAttrsToList (n: v: ''
                    mkdir -p $sourceRoot/${n}
                    cp -r ${v}/* $sourceRoot/${n}
                  '') deps)}
                chmod u+w -R .
              '';
            configurePhase = ''
            ${pkgs.tree}/bin/tree .
            gn gen out/Release --args="is_component_build=false is_debug=false angle_assert_always_on=false angle_enable_cl=true angle_build_tests=false angle_enable_swiftshader=false use_android_unwinder_v2=false symbol_level=0" 
            '';
            buildPhase = ''
              ls -al
              ninja -C out/release libANGLE_static
              ninja -C out/release libGLESv2_static
              ninja -C out/release libEGL_static
              ${pkgs.tree}/bin/tree .
            '';
            installPhase = ''
              mkdir -p $out/{lib,include}
              shopt -s globstar
              cp -R build/release/arm64/* $out/lib/
              cp -R include/* $our/include/
            '';
          };
        };
        aarch64-linux = {
          demo = 
            self.nixosConfigurations.demo.config.system.build.vm.overrideAttrs(_: old: {
              # FIXME MASSIVE HACK
              # requires akirakyle/homebrew-qemu-virgl
              # that has virgl embedded
              # can't build it easily yet
              meta.mainProgram = "run-darwin-vm";
              buildCommand = ''
                ${old.buildCommand}
                sed 's#/nix/store/\(.*\)-qemu-\(.*\)/bin#/opt/homebrew/bin#g;s#-device virtio-gpu-pci##g' \
                  $out/bin/run-demo-vm > $out/bin/run-darwin-vm
                chmod +x $out/bin/run-darwin-vm
              '';
            });
        };
      };
    };
}

