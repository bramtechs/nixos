{ config, lib, pkgs, callPackage, ... }:

{
  # system packages
  environment.systemPackages = with pkgs; [
    vmware-workstation
    jetbrains.phpstorm
    jetbrains.clion
    jetbrains.idea-community
    android-studio
    docker-compose
    codeblocks

    # creation
    obs-studio

    # deps
    nodejs_21

    rustc
    cargo
    rustfmt

    # python
    spotdl

    # shell tools
    fd
    ripgrep
    python311Packages.pynvim
    unzip
    rsync

        osu-lazer-bin
    spotify
    gimp
    libreoffice
    vlc    

    blender
    inkscape

    evince
    remmina
    firefox
    keepassxc

    gnome.gnome-font-viewer
    eclipses.eclipse-java

    mpv
    audacious
  ];
}
