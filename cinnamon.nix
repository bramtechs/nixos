{ config, lib, pkgs, ... }:

{
    services.xserver = {
      displayManager.lightdm.enable = true;
      windowManager.exwm.enable = true;
      desktopManager = {
        cinnamon.enable = true;
      };
    };
    services.displayManager.defaultSession = "cinnamon";

    environment.systemPackages = with pkgs; [
        gnome.gnome-system-monitor
    ];
}
