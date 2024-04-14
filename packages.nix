{ config, lib, pkgs, callPackage, ... }:

{
  # unfree
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (_: true);

  # system packages
  environment.systemPackages = with pkgs; [

    vmware-workstation
    
    # CODING
    git
    neovim
    tmux
    rnix-lsp
    gh

    tpm2-tools
    tpm2-tss
    inetutils

    jetbrains.phpstorm
    jetbrains.clion
    jetbrains.idea-community
    android-studio
    docker-compose
    codeblocks

    # creation
    obs-studio

    # deps
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

    # compilers
    gcc

    # python
    spotdl

    # UTILS
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

    # i3
    nitrogen
    scrot
    redshift
    i3blocks
    i3status-rust
    xfce.xfce4-power-manager
    blueman
    networkmanagerapplet

    # GUI
    gnome.gnome-disk-utility
    xfce.xfce4-terminal
    xterm
    pavucontrol

    # 'RICING'
    lxappearance

    # shell tools
    fd
    ripgrep
    python311Packages.pynvim
    unzip
    rsync
  ];
}
