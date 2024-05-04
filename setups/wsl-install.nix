{ config, lib, pkgs, ... }:

{
  imports =
    [
        <nixos-wsl/modules>
      ../configuration.nix
      ../packages.nix
    ];

    wsl.enable = true;
    wsl.defaultUser = "bram";

    hardware.pulseaudio.enable = false; # remove annoying beeps
}
