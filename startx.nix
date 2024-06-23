{ config, lib, pkgs, ... }:

{
    services.xserver = {
      displayManager.startx.enable = true;
    };

    environment.systemPackages = with pkgs; [
        gnome.gnome-system-monitor
        gnome.gnome-terminal
    ];

    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;
}
