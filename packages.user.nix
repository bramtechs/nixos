{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    osu-lazer-bin
    discord
    spotify
    gimp
    libreoffice
    vlc

    blender
    inkscape

    evince
    remmina
    google-chrome
    firefox
    keepassxc

    adapta-gtk-theme
    ubuntu_font_family
    ubuntu-themes

    gradle
    jdk17
    dotnet-sdk_8

    flameshot
    gnome.nautilus
    resilio-sync

    eclipses.eclipse-java

    mpv
    cmus
    audacious
  ];
}