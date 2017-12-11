{ pkgs, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices, imagemagick }:

let
    repoName = "chunkwm";
    repoV = "0.2.36";
    repoSha = "1whsp8cl00facspmvrmiw7zanrk75ixh287c4xhia9h9w3dsjcv6";
    blurRepoOwner =  "splintah";
    blurRepoName = "blur";
    blurRepoV = "0.1.3";
    blurRepoSha = "0lvywxrvyk1s9f0k3j9p7jbl6mnb0vzscdnz76glnkzcvqwc97ip";
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

    purify = pkgs.callPackage ./plugin.nix {
      cfg = { name = "purify";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };

    bar = pkgs.callPackage ./plugin.nix {
      cfg = { name = "bar";
          version = repoV;
          sha256 = repoSha;
        };
      inherit Carbon Cocoa ApplicationServices;
    };

    # blur = pkgs.callPackage ./plugin.nix {
    #   cfg = { name = "blur";
    #       owner = blurRepoOwner;
    #       repo = blurRepoName;
    #       version = blurRepoV;
    #       sha256 = blurRepoSha;
    #     };
    #   inherit Carbon Cocoa ApplicationServices imagemagick;
    # };
}
