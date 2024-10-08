{ pkgs, ... }:

let doom = import (import ./modules.nix { }).doomhowl { };
in {
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
    scrot
    nil # nix lsp
    nixfmt-classic
    tree

    # custom packages
    doom.wolfpack
    cmake
    gdb
    lldb

    # reminder to use custom shells
    # for compilers as installing
    # clang in here will cause weird
    # linking issues
  ];
}
