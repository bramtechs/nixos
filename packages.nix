{ config, lib, pkgs, callPackage, ... }:

{
  # unfree
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (_: true);

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
  documentation.dev.enable = true;
  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix
    git
    neovim
    tmux
    cmake
    gcc
    php83
    php83Packages.composer  
    gcc
    spotdl
    wget
    curl
    neofetch
    htop
    bmon
    ranger
    gnumake
    ffmpeg
    yt-dlp
    pandoc
    cmus
    rclone
    dos2unix
    emacs29-gtk3
  ];
}
