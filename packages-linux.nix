{ config, lib, pkgs, callPackage, ... }:

{
    # Linux packages that I don't want on Mac or aren't available.

    # system packages
    documentation.dev.enable = true;
    environment.systemPackages = with pkgs; [
        gnome.gnome-disk-utility
        gnome.nautilus
        gnome.gnome-clocks
        gnome.ghex
        gnome.gnome-disk-utility

        tpm2-tools
        tpm2-tss

        flameshot
        rnix-lsp
        nodejs_21

        #google-chrome
        adapta-gtk-theme
        ubuntu_font_family
        ubuntu-themes
        gitg
        xterm
        pavucontrol
        lxappearance
        discord
        libnotify
        xclip
        gdb
        dmd
        gf
        iotop
        playerctl
        rustc
        cargo
        rustfmt
        haxe
        inetutils
    ];
}
