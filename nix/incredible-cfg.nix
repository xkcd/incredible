{ config, pkgs, ... }:
{
  imports = [
    ./users/deploy.nix
  ];

  system.stateVersion = "22.05";

  networking.hostId = "a1c232ce";

  fileSystems."/" = {
    device = "rpool/save/root";
    fsType = "zfs";
  };
  fileSystems."/home" = {
    device = "rpool/save/home";
    fsType = "zfs";
  };
  fileSystems."/var/www" = {
    device = "rpool/save/var/www";
    fsType = "zfs";
  };
  fileSystems."/save" = {
    device = "rpool/save";
    fsType = "zfs";
    options = [ "ro" ];
  };
  fileSystems."/var" = {
    device = "rpool/save/var";
    fsType = "zfs";
  };
  fileSystems."/var/log/journal" = {
    device = "rpool/local/journal";
    fsType = "zfs";
  };
  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };
  fileSystems."/boot" = {
    # The ZFS image uses a partition labeled ESP whether or not we're
    # booting with EFI.
    device = "/dev/disk/by-label/ESP";
    fsType = "vfat";
  };

  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.expandOnBoot = [ "rpool" ]; #uncommenting this brings in all of X11 via cloud-utils and its dep qemu.

  nix = {
    package = pkgs.nixVersions.stable;
    settings = {
      # so that deploy user can copy into to the nix store
      trusted-users = [ "deploy" ];

      # Haskell.nix cache
      substituters = [ "https://cache.iog.io" ];
      trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    };
    extraOptions = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes
    '';
  };

  # Used for the deploy user to run non-interactive remote nixos-rebuild
  security.sudo.extraRules= [ {
    users = [ "deploy" ];
    commands = [ {
      command = "ALL" ;
      options= [ "NOPASSWD" ];
      }];
  }];

  environment.systemPackages = with pkgs; [
    rxvt_unicode.terminfo
    wget htop dstat ethtool tmux git git-lfs emacs-nox
    rsync rrsync
  ];

  sound.enable = false;
  services.xserver.enable = false;

  users.mutableUsers = false;
  users.groups.site_deploy = {};

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
    allowedUDPPorts = [    443 ];
  };
  
  services.openssh.enable = true;
}
