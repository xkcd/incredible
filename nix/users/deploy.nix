{ pkgs, ... }:
{
  programs.zsh.enable = true;
  users.users.deploy = {
     isNormalUser = true;
     extraGroups = [ "wheel" ];
     shell = pkgs.zsh;
     openssh.authorizedKeys.keys =
       [
        # Add a public key here
       ];
   };
}
