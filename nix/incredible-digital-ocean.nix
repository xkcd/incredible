{ config, pkgs, ... }:
{
  imports = [
    ./digital-ocean/digital-ocean-custom-image.nix
  ];

  virtualisation = {
    digitalOceanImage = {
      rootSize = 8192; # might need to be bigger
      # configFile = ./basicly-blank-digital-ocean-cfg.nix;
      datasets = {
        "rpool/save/root".mount = "/";
        "rpool/save/home".mount = "/home";
        "rpool/save/var".mount = "/var";
        "rpool/save".mount = "/save";
        "rpool/save/var/www".mount = "/var/www";
        "rpool/local/journal".mount = "/var/log/journal";
        "rpool/local/nix".mount = "/nix";
      };
    };
  };
}
