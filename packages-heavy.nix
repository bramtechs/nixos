{ ... }:

let
  pkgs = import <nixpkgs> {
    config = {
      allowUnfree = true;
    };
  };
in {
  # system packages
  environment.systemPackages = with pkgs; [
    jetbrains.clion
    jetbrains.idea-community-bin
    android-studio
    android-studio-tools
    docker-compose
    bruno
    dbeaver-bin
    obs-studio
    spotdl
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
    mpv
    google-chrome
    discord
    spotify
  ];
}
