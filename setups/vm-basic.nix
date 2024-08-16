{ lib, pkgs, ... }:

with lib;

{
  imports =
  [     ];

  powerManagement.enable = false;
  system.stateVersion = mkDefault "18.03";

  time.timeZone = "Europe/Brussels";

  environment.systemPackages = with pkgs; [
    vim
  ];

  console = { keyMap = "us"; };

  users.users.virtual = {
    isNormalUser = true;
    initialPassword = "virtual";
    extraGroups = [
      "sudo"
    ];
  };

  services.openssh.enable = true;
}
