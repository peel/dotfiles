{ config, pkgs, ... }:

let
  mkCache = url: key: { inherit url key; };
  caches =
    let
      nixos = mkCache "https://cache.nixos.org" "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
      peel = mkCache "https://peel.cachix.org" "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE=";
      cachix = mkCache "https://cachix.cachix.org" "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=";
      nix-tools = mkCache "https://nix-tools.cachix.org" "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A=";
      nix-community = mkCache "https://nix-community.cachix.org" "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
      iohk = mkCache "https://hydra.iohk.io" "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
      iohk-cachix = mkCache "https://iohk.cachix.org" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=";
    in [ nixos peel cachix nix-community nix-tools ];
in {
  imports = [
    #./emacs.nix
    ./fonts.nix
    ./git.nix
    ./gnupg.nix
    ./packages.nix
    ./shells.nix
  ];

  nix = {
    package = pkgs.nixUnstable;
    binaryCaches = builtins.map (x: x.url) caches;
    binaryCachePublicKeys = builtins.map (x: x.key) caches;
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    extraOptions = let
      maxJobs = "16";
      arch = if pkgs.stdenv.isAarch64 then "aarch64" else "x86_64";
      crossBuilder = if pkgs.stdenv.isDarwin then "ssh-ng://builder@localhost ${arch}-linux /etc/nix/builder_ed25519 ${maxJobs} - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=" else "";
      x86Builder = if pkgs.stdenvNoCC.isDarwin && pkgs.stdenvNoCC.isAarch64 then "ssh-ng://peel@nuke.local x86_64-linux /Users/peel/.ssh/local_id_rsa 16 - - - AAAAB3NzaC1yc2EAAAADAQABAAACAQDQRppOSxqH3JEzMyCIBbmCYWEuOg0+vR2l4GVV8ir7xsMgvsVmcFskUBUp4I/9LohouwycTiL/cnLz5gQVcTHmo3RGMlxJIeBobQ2WuZQqEkdRPL/QmNhiRTk/Pw+0T1u0Wt/CoktK/LG/8B3osDUbUOn5GmV1qg0gtyFcid7QFPeIIW9CEIxqpPRSoVuxSg2lGijdmGD4YPXiA6Dr6AZPu93Ujb6tQpTaqCECYd3YNGTMHE+z8qGxA0pcjP6n6MecIQzOEai+p7hT4i6rJ2lkVUGLIQu5k1Be+JvMD3/j5lGpS+8zt2dAYhdajY69PNezZSrZfm4end/YOuz/LAwgHJkYIPIYXQU3E3smU1bWOfH/Fo6NfrR1IQUxi5ERFvgxT/meRzaW5Jf1AcKH3kJSLlgivTQVbdaaBHLhEL22J+fs8y1z7f4dDN+0Y94kv9tK0CtjEEaKpisSUp60dUrm+d4c1mqVInPjyd3N3Bkf3Q4tLFGGj3zQT2Vuw0GwCSGNXKzH0j2YkuArnW+G7yk8L10OkyOXse+Sb+juR9pHhssflMdAgQ8zvtGAU6k83I1EWqvYpaPc+x/KExuTDPR8TSvnXxZiAUu+cwnwG7ZLElmte+cRXKJIKX4VRMeviTDKfO8f0Mz8DT4j9T1AR7xlgmbwEjSWJ9B8MUgsTmWX+w==" else "";
    in ''
      experimental-features = nix-command flakes
      builders = @/etc/nix/machines ${crossBuilder} ${x86Builder}
      builders-use-substitutes = true
    '';
  };
  time.timeZone = "Europe/Warsaw";
}
