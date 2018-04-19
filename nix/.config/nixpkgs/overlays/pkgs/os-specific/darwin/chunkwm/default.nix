{ newScope, callPackage, stdenv, fetchFromGitHub, Carbon, Cocoa, ApplicationServices, imagemagick }:

let
  repoName = "chunkwm";
  repoV = "0.3.4";
  repoSha = "0x3qy66r6wbg51wfdpwgk1kz7qwx2naivxmj2dj0x0arbh721v3p";

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
