{ pkgs, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

let
    repoV = "0.2.5";
    repoSha = "0znqvf3xis0fy8f27z3nm1yhjk2643s9lr478rv3ialvg7a097q0";
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
}
