{ config, lib, pkgs, callPackage, ... }:

let
  wolfpack = import (pkgs.fetchFromGitHub {
    owner = "bramtechs";
    repo = "wolfpack";
    rev = "fb009a1415b228d1d559440feaee34ed4530d36d";
    hash = "sha256-fIxBKYUXw7KCdwc5qaUfWGJg9oR28PHhXyaGYZu2g7s=";
  });
in {
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
    wolfpack
  ];
}
