{ config, modulesPath, pkgs, lib,... }:
{
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
    "${modulesPath}/virtualisation/qemu-vm.nix"
  ];

  services.qemuGuest.enable = true;

  services.getty.autologinUser = config.users.users.deploy.name;
  users.mutableUsers = lib.mkForce true;

  virtualisation = {
    memorySize = 2048; # MB
    diskSize = 8000; # MB
    cores = 2;
    forwardPorts = [
      { from = "host"; host.port = 8889; guest.port = 8889; }
      { from = "host"; host.port = 8888; guest.port = 8888; }
    ];
  };
}
