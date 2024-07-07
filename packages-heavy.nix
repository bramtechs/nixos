{ config, lib, pkgs, callPackage, ... }:

{
  # system packages
  environment.systemPackages = with pkgs; [
    vmware-workstation
    # jetbrains.phpstorm
    jetbrains.clion
    jetbrains.idea-community
    # jetbrains.rider

    # microsoft-edge
    # azuredatastudio

    docker-compose
    # codeblocks
    # thunderbird
    # element-desktop
    bruno
    dbeaver-bin
    obs-studio
    nodejs
    
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

    gimp
    libreoffice
    vlc

    blender
    inkscape

    evince
    remmina
    keepassxc

    gnome.gnome-font-viewer
    # eclipses.eclipse-java
    mpv

    # distrobox
    google-chrome
    discord
    spotify
  ];
}
