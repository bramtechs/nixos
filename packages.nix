{ config, lib, pkgs, callPackage, ... }:

{
  # unfree
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (_: true);

  # system packages
  environment.systemPackages = with pkgs; [
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
    neofetch
    dos2unix
  ];
}
