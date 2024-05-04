incredible-frontend: { config, pkgs, lib, ... }:
with lib;
let cfg = config.services.incredible-frontend; in
{
  options = {
    services.incredible-frontend = {
      enable = mkEnableOption "incredible frontend";
    };
  };

  config = mkIf cfg.enable {

    networking.firewall = {
      allowedTCPPorts = [ 8889 ];
    };

    services.nginx = {
      enable = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      virtualHosts = {
        localhost = {
          default = true;
          extraConfig = ''
            charset UTF-8;
          '';
          locations."/" = {
            root = "${incredible-frontend}/lib/node_modules/incredible/built/";
            extraConfig = ''
              add_header Access-Control-Allow-Origin *;
              '';
          };
          listen = [
            {
              addr = "0.0.0.0";
              port = 8889;
            }
          ];
        };
      };
    };

  };
}
