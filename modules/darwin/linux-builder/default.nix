{ pkgs, ... }:

let
  # aarch64-darwin or x86_64-darwin
  darwinSystem = pkgs.stdenv.hostPlatform.system;
  # s/darwin/linux
  linuxSystem = builtins.replaceStrings [ "darwin" ] [ "linux" ]
    darwinSystem;

  # Attribute set describing the Linux builder
  linuxBuilder = rec {
    dataDir = "/var/lib/nixos-builder";
    logPath = "/var/log/linux-builder.log";
    system = linuxSystem; # {x86_64|aarch64}-linux
    port = 31022;

    # The builder VM itself
    builder = (import "${pkgs.path}/nixos" {
      system = linuxSystem; # {x86_64|aarch64}-linux
      configuration = ({ modulesPath, lib, ... }: {
        imports = [ "${modulesPath}/profiles/macos-builder.nix" ];
        virtualisation = {
          host.pkgs = pkgs;
          forwardPorts = lib.mkForce [{
            from = "host";
            host.address = "127.0.0.1";
            host.port = port;
            guest.port = 22;
          }];
        };
      });
    }).config.system.build.macos-builder-installer;
  };

  # A Bash script for running the builder
  runLinuxBuilderScript = pkgs.writeShellScriptBin "run-linux-builder" ''
    set -uo pipefail
    trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
    IFS=$'\n\t'
    mkdir -p "${linuxBuilder.dataDir}"
    cd "${linuxBuilder.dataDir}"
    ${linuxBuilder.builder}/bin/create-builder
  '';
in
{
  environment = {
    # SSH configuration
    etc = {
      "nix/ssh_config".text = ''
        Host linux-builder
          User builder
          HostName 127.0.0.1
          Port ${toString linuxBuilder.port}
          IdentityFile ${linuxBuilder.dataDir}/keys/builder_ed25519
          UserKnownHostsFile /etc/nix/ssh_known_hosts
      '';

      "nix/ssh_known_hosts".text = ''
        [127.0.0.1]:31022 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJBWcxb/Blaqt1auOtE+F8QUWrUotiC5qBJ+UuEWdVCb
      '';
    };
  };

  launchd.daemons.linux-builder = {
    command = "${runLinuxBuilderScript}/bin/run-linux-builder";
    path = with pkgs; [ "/usr/bin" coreutils nix ];
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = linuxBuilder.logPath;
      StandardErrorPath = linuxBuilder.logPath;
    };
  };

  nix = {
    buildMachines = [
      # FIXME
      {
        hostName = "ssh-ng://peel@nuke.local";
        maxJobs = 16;
        supportedFeatures = [ "kvm" ];
        system = "x86_64-linux";
        #FIXME
        publicHostKey = ''
AAAAB3NzaC1yc2EAAAADAQABAAACAQDQRppOSxqH3JEzMyCIBbmCYWEuOg0+vR2l4GVV8ir7xsMgvsVmcFskUBUp4I/9LohouwycTiL/cnLz5gQVcTHmo3RGMlxJIeBobQ2WuZQqEkdRPL/QmNhiRTk/Pw+0T1u0Wt/CoktK/LG/8B3osDUbUOn5GmV1qg0gtyFcid7QFPeIIW9CEIxqpPRSoVuxSg2lGijdmGD4YPXiA6Dr6AZPu93Ujb6tQpTaqCECYd3YNGTMHE+z8qGxA0pcjP6n6MecIQzOEai+p7hT4i6rJ2lkVUGLIQu5k1Be+JvMD3/j5lGpS+8zt2dAYhdajY69PNezZSrZfm4end/YOuz/LAwgHJkYIPIYXQU3E3smU1bWOfH/Fo6NfrR1IQUxi5ERFvgxT/meRzaW5Jf1AcKH3kJSLlgivTQVbdaaBHLhEL22J+fs8y1z7f4dDN+0Y94kv9tK0CtjEEaKpisSUp60dUrm+d4c1mqVInPjyd3N3Bkf3Q4tLFGGj3zQT2Vuw0GwCSGNXKzH0j2YkuArnW+G7yk8L10OkyOXse+Sb+juR9pHhssflMdAgQ8zvtGAU6k83I1EWqvYpaPc+x/KExuTDPR8TSvnXxZiAUu+cwnwG7ZLElmte+cRXKJIKX4VRMeviTDKfO8f0Mz8DT4j9T1AR7xlgmbwEjSWJ9B8MUgsTmWX+w==
        '';
        #FIXME
        sshKey = "/Users/peel/.ssh/local_id_rsa";
      }
      {
      hostName = "ssh-ng://linux-builder";
      maxJobs = 16;
      # This is cheating: KVM isn't actually available (?) but QEMU falls back to "slow mode" in this case
      supportedFeatures = [ "kvm" ];
      system = linuxSystem; # {x86_64|aarch64}-linux
    }];
    distributedBuilds = true;
    envVars = { NIX_SSHOPTS = "-F /etc/nix/ssh_config"; }; # See the config above
  };

  # Make sure that the Nix daemon is enabled in the nix-darwin config
  services.nix-daemon.enable = true;
}
