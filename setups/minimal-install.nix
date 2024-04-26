{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      ../configuration.nix
      ../cinnamon.nix
      ../packages.nix
      ../packages-linux.nix
    ];
}