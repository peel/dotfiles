{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.weechat;
  getLuaPath = lib : dir : "${lib}/${dir}/lua/${pkgs.luaPackages.lua.luaversion}";
  makeSearchPath = lib.concatMapStrings (path:
    " --search " + (getLuaPath path "share") +
    " --search " + (getLuaPath path "lib")
  );
  weechat = pkgs.weechat.override {
    configure = {availablePlugins,...}: {
      plugins = with availablePlugins; [
        (python.withPackages (ps: with ps; [websocket_client xmpppy]))
        lua
      ];
    };
  };
in
{
  options = {
    services.weechat = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable weechat relay service.
        '';
      };
      home = mkOption {
        default = "/var/weechat";
        description = ''
          Where weechat configuration is stored.
        '';
      };
      session = mkOption {
        default = "/tmp/tmux-weechat";
        description = ''
          Where store tmux weechat session.
        '';
      };
      portsToOpen = mkOption {
        default = [];
        description = ''
          Relay ports to open.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = cfg.portsToOpen;
    systemd.user.services."weechat" = {
      enable = true;
      description = "Weechat relay service";
      wantedBy = [ "default.target" ];
      environment = {
        LANG = "en_US.utf8";
        LC_ALL = "en_US.utf8";
        TERM = "${pkgs.rxvt_unicode.terminfo}";
        LUA_PATH = makeSearchPath [ pkgs.luaPackages.cjson ];
      };
      path = [
        weechat
        pkgs.tmux
        pkgs.rxvt_unicode.terminfo
        pkgs.luaPackages.cjson
      ];
      restartIfChanged = true;
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = "yes";
      serviceConfig.KillMode = "none";
      serviceConfig.WorkingDirectory = "${cfg.home}";
      serviceConfig.ExecStart = "${pkgs.tmux}/bin/tmux -2 -S ${cfg.session} new-session -d -s weechat '${weechat}/bin/weechat -d ${cfg.home}'";
      serviceConfig.ExecStop = "${pkgs.tmux}/bin/tmux -S ${cfg.session} kill-session -t weechat";
    };
  };
}
