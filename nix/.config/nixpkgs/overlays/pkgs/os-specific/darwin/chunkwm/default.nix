{ newScope, callPackage, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices, imagemagick }:

let
  repoName = "chunkwm";
  repoV = "0.3.3";
  repoSha = "0h5sqljcwlbq2h2nrqd90rvn074ima39j10j4skfxparr9mzs21r";

  self = chunkwm;
  callPackage = newScope self;

  chunkwm = with self; {
      core = callPackage ./core.nix {
        cfg = { name = "core";
            version = repoV;
            sha256 = repoSha;
          };
        inherit Carbon Cocoa;
      };

      border = callPackage ./plugin.nix {
        cfg = { name = "border";
            version = repoV;
            sha256 = repoSha;
          };
        inherit Carbon Cocoa ApplicationServices;
      };

      ffm = callPackage ./plugin.nix {
        cfg = { name = "ffm";
            version = repoV;
            sha256 = repoSha;
          };
      inherit Carbon Cocoa ApplicationServices;
      };

      tiling = callPackage ./plugin.nix {
        cfg = { name = "tiling";
            version = repoV;
            sha256 = repoSha;
          };
        inherit Carbon Cocoa ApplicationServices;
      };

      purify = callPackage ./plugin.nix {
        cfg = { name = "purify";
            version = repoV;
            sha256 = repoSha;
          };
        inherit Carbon Cocoa ApplicationServices;
      };

      bar = callPackage ./plugin.nix {
        cfg = { name = "bar";
            version = repoV;
            sha256 = repoSha;
          };
        inherit Carbon Cocoa ApplicationServices;
      };
  }; in chunkwm
