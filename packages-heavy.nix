{ ... }:

let
  unstable = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "fa4b81d8e9ad1f7b35c9f2cdb41318e3487eaf9a";
    hash = "sha256-oXeiqYtpuGiNCIFJ0TNP1YWtPCLoENXknh90ztBmlCE=";
  }) { config = { allowUnfree = true; }; };

  pkgs = import <nixpkgs> {
    config = {
      permittedInsecurePackages = [ "pulsar-1.117.0" ];
      allowUnfree = true;
    };
  };
in {
  # system packages
  environment.systemPackages = with pkgs; [
    vmware-workstation
    # jetbrains.phpstorm
    unstable.jetbrains.clion
    jetbrains.idea-community-bin
    # jetbrains.rider

    # microsoft-edge
    # azuredatastudio

    unstable.android-studio
    docker-compose
    # codeblocks
    # thunderbird
    # element-desktop
    bruno
    dbeaver-bin
    obs-studio
    nodejs
    pulsar

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
