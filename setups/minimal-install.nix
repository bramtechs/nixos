{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      ../configuration.nix
      ../cinnamon.nix
      ../packages.nix
    ];
}