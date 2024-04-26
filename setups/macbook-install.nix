{ config, lib, pkgs, callPackage, ... }:
{
  imports =
    [
      ../configuration-mac.nix
    ];

    system.stateVersion = 4;
}