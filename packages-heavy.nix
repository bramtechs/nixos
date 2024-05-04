{ config, lib, pkgs, callPackage, ... }:

{
  # fonts
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      font-awesome
      alacritty
      anonymousPro
      corefonts
      dejavu_fonts
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };

  # system packages
  environment.systemPackages = with pkgs; [
    vmware-workstation
    jetbrains.phpstorm
    jetbrains.clion
    jetbrains.idea-community
    jetbrains.rider
    
    azuredatastudio
    androidStudioPackages.dev
    android-tools
    
    docker-compose
    codeblocks
    thunderbird
    element-desktop
    bruno
    dbeaver
    
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
