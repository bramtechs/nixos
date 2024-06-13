{ config, lib, pkgs, ... }:

{
    services.xserver = {
      displayManager.sddm.enable = true;
      windowManager.exwm.enable = true;
      desktopManager = {
        cinnamon.enable = true;
      };
      displayManager.defaultSession = "cinnamon";
    };

    environment.systemPackages = with pkgs; [
        gnome.gnome-system-monitor
    ];
}
