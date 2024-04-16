{ config, lib, pkgs, callPackage, ... }:

{
    imports =
        [
        ../configuration.nix
        ../cinnamon.nix
        ];

    services.xserver.videoDrivers = ["nvidia"];

     hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
}