{ pkgs, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

let
    repoV = "0.2.31";
    repoSha = "1h3phbj8hw6m45p1nrmnmma3bsrhg8614an9pqdzk49k35i83a35";
in
stdenv.mkDerivation rec {

    core = pkgs.callPackage ./core.nix {
      cfg = { name = "core";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa;
    };

    border = pkgs.callPackage ./plugin.nix {
      cfg = { name = "border";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };

    ffm = pkgs.callPackage ./plugin.nix {
      cfg = { name = "ffm";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };

    tiling = pkgs.callPackage ./plugin.nix {
      cfg = { name = "tiling";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };

    transparency = pkgs.callPackage ./plugin.nix {
      cfg = { name = "transparency";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };
}
