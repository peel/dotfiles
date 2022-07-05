flakes @ { self, nixpkgs }:

{ config, lib, pkgs, ... }:
let
  s = import ../s.nix;
in {
  imports = [
    "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
  ];

  ec2.hvm = true;

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };

  nix.gc = {
    automatic = true;
    dates = "19:30";
  };

  environment.systemPackages = [
    pkgs.cloud-utils
  ];

  system.stateVersion = "22.05";

  security.sudo.wheelNeedsPassword = false;
  nix.trustedUsers = ["@wheel"];
  
  users.users = {
    peel = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [
        s.sshpeel
      ];
    };
    root.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC07OpswXChBtHfoETqKRXdDA/Hzuw/2E7bi9Zwn6tuMLHuE9t69HunLLFPfiPjbUXXKjPNIW0E+5XIwo3X8vYqIY4bsc47uoMXSA6FER/FI8gNY3qRrRri5h23TzG5Dg/1s9ztM1rp9NFMSUf8XmiLkZZkSQme41OqLhy9d7YZak76NLIsEMp9inCdGypRrKupUQ8bWIL9kxVM1i90yPdqPKpRJ7JvRhL1L2aGIYyLjHKwyvT1oPvfbKM2vvSqtiabRuBwSWFM7Pm49HmEeasabKvxCy0xO5YryD/QlKtnq8oamS/Sp7reHxiVTxc87ZKCWroHo5hrGZvmbMROXS63"
    ];
  };
  services.openssh.passwordAuthentication = false;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 53 80 443 ];
  networking.firewall.allowedUDPPorts = [ 53 config.services.tailscale.port ];

  services.tailscale.enable = true;
  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";
    after = [ "network-pre.target" "tailscale.service" ];
    wants = [ "network-pre.target" "tailscale.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = with pkgs; ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${pkgs.tailscale}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${pkgs.tailscale}/bin/tailscale up -authkey ${s.tsvps}
    '';
  };
  
  security.acme = {
    email = "plimanowski@pm.me";
    acceptTerms = true;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    clientMaxBodySize = "2G";
    statusPage = true;

    virtualHosts."px.fff666.org" = {
      default = true;
      enableACME = true;
      addSSL = true;
      http2 = true;
      locations."/" = {
        proxyPass = "http://192.168.100.2";
        proxyWebsockets = true;
        extraConfig = ''
          add_header X-Robots-Tag: noindex;
        '';
      };
    };
  };
}
