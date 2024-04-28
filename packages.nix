{ config, lib, pkgs, callPackage, ... }:

{
  # unfree
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (_: true);

  # system packages
  documentation.dev.enable = true;
  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix
    git
    neovim
    tmux
    rnix-lsp
    tpm2-tools
    tpm2-tss
    inetutils
    nodejs_21
    rustc
    cargo
    rustfmt
    cmake
    gcc
    gdb
    dmd
    gf
    php83
    php83Packages.composer
    haxe
    gcc
    spotdl
    wget
    curl
    neofetch
    htop
    bmon
    iotop
    playerctl
    ranger
    gnumake
    libnotify
    xclip
    ffmpeg
    yt-dlp
    pandoc
    gnome.gnome-disk-utility
    xterm
    pavucontrol
    lxappearance
    gitg
    adapta-gtk-theme
    ubuntu_font_family
    ubuntu-themes
    flameshot
    gnome.nautilus
    gnome.gnome-clocks
    gnome.ghex
  ];
}
