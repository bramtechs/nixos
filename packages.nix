{ config, lib, pkgs, callPackage, ... }:

let
  doom = import <doomhowl> {};
in
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
    man-pages
    man-pages-posix
    git
    neovim
    tmux
    cmake
    gcc
    php83
    php83Packages.composer
    spotdl
    wget
    curl
    htop
    bmon
    ranger
    gnumake
    ffmpeg
    yt-dlp
    pandoc
    texliveMedium
    cmus
    rclone
    dos2unix
    emacs29-gtk3
    scrot
    nil # nix lsp

    # custom packages
    doom.raylib
    cmake
    gdb
    lldb

    # reminder to use custom shells
    # for compilers as installing
    # clang in here will cause weird
    # linking issues
   ];
}
