{ config, lib, pkgs, callPackage, ... }:

{
  nixpkgs.config.bitlbee.enableLibPurple = true;

  services.bitlbee = {
    enable = true;
    libpurple_plugins = with pkgs; [
      pidginPackages.purple-discord 
    ];
  };

  networking.firewall.deniedTCPPorts = [ 6667 ];
  
  environment.systemPackages = with pkgs; [
    pidgin
  ];
}
