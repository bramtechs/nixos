{ config, lib, pkgs, callPackage, ... }:

{
  imports =
    [
      ../configuration.nix
      ../i3.nix
    ];
}