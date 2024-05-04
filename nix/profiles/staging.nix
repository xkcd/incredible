{ config, lib, pkgs, ... }:

{
  services.incredible-server = {
    enable = true;
    config = ../../config/incredible.toml;
    machine = ../../config/machine.json;
  };

  services.incredible-frontend = {
    enable = true;
  };
}
