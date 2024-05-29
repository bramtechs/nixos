{ config, lib, pkgs, callPackage, ... }:

{
    # Linux packages that I don't want on Mac.

    # system packages
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

        adapta-gtk-theme
        ubuntu_font_family
        ubuntu-themes
        gitg
        xterm
        pavucontrol
        lxappearance
        
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
