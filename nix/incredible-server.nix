incredible-server: { config, pkgs, lib, ... }:

with lib;
let cfg = config.services.incredible-server; in
{
  options = {
    services.incredible-server = {
      enable = mkEnableOption "incredible web server";

      config = mkOption {
        type = types.path;
        description = "Path to incredible.toml";
      };

      machine = mkOption {
        type = types.path;
        description = "Path to machine.json";
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 8888 ];
    };

    services.redis.servers."incredible" = {
      enable = true;
      port = 6379;
    };

    systemd.services.incredible = {
      description = "Incredible web server";
      after = [ "network.target" "redis-incredible.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${incredible-server}/bin/incredible-server --config ${cfg.config} --machine ${cfg.machine}";
        LimitNOFILE = 1000000;
        WorkingDirectory = "/home/incredible";
        Restart = "always";
        RestartSec = 5;
        User = "incredible";
        Group = "incredible";
      };
    };

    users.users.incredible = {
      isSystemUser = true;
      home = "/home/incredible";
      group = "incredible";
      createHome = true;
    };

    users.groups.incredible = {
      members = [ "incredible" ];
    };

  };
}
