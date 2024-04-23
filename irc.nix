{ config, lib, pkgs, callPackage, ... }:

{
  nixpkgs.config.bitlbee.enableLibPurple = true;

  services.bitlbee = {
    enable = false; # disabled for now
    libpurple_plugins = with pkgs; [
      pidginPackages.purple-discord 
    ];
  };

}
