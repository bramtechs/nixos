{ config, lib, pkgs, callPackage, ... }:

{
  # Linux-specific packages, not available on MacOS.

  # system packages
  documentation.dev.enable = true;
  environment.systemPackages = with pkgs; [
    gnome.gnome-disk-utility
    gnome.nautilus
    gnome.gnome-clocks
    gnome.ghex
    gnome.gnome-disk-utility
    baobab
    lm_sensors
    flameshot
    mission-center
    adapta-gtk-theme
    ubuntu_font_family
    ubuntu-themes
    gitg
    xterm
    pavucontrol
    lxappearance
    acpi
    libnotify
    xclip
    iotop
    playerctl
    inetutils
    feh
    sloccount
  ];
}
