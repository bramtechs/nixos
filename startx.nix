{ config, lib, pkgs, ... }:

{
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  services.xserver = {
    displayManager.startx.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnome.gnome-system-monitor
    xfce.xfce4-terminal

    openbox
    openbox-menu
    xfce.mousepad
    xorg.xbacklight
    
    picom

    obconf
    lxmenu-data

    cinnamon.nemo-with-extensions
    dmenu
    nitrogen
  ];

}
