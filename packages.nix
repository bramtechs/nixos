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
    haxe
    gcc
    php83
    php83Packages.composer
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
    cmatrix
    hollywood
    yt-dlp
    playerctl
    pandoc
    gnome.gnome-disk-utility
    xterm
    pavucontrol
    lxappearance
    discord
    gitg
    cmus
    google-chrome
    adapta-gtk-theme
    ubuntu_font_family
    ubuntu-themes
    flameshot
    gnome.nautilus
  ];
}
