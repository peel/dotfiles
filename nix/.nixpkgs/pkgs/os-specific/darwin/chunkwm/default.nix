{ pkgs, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices }:

let
    repoV = "0.2.11";
    repoSha = "1p14jajn6xfh1d3qkyhhyxpzmw6dxjx121hakd4pyirfkjarfsrv";
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
