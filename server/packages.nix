{ config, lib, pkgs, callPackage, ... }:

{
  environment.systemPackages = with pkgs; [
    git
    man-pages
    tmux
    htop
    bmon
    dos2unix
    rsync
    ranger
    wget
    curl
    vim
    gnumake
    nixfmt-classic
    docker-compose
    ccache
  ];
}
