{ config, lib, pkgs, ... }:

{
  imports =
    [
        <nixos-wsl/modules>
        <home-manager/nixos>
      ../configuration.nix
      ../packages.nix
    ];

    wsl.enable = true;
    wsl.defaultUser = "bram";

    hardware.pulseaudio.enable = false; # remove annoying beeps

    home-manager.users.bram = { config, lib, pkgs, ... }: {
      imports = [
        ../bram.nix
      ];
    };
}
