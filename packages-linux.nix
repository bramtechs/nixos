{ config, lib, pkgs, callPackage, ... }:

{
    # Linux-specific packages, not available on MacOS.

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
