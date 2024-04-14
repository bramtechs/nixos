{ config, lib, pkgs, callPackage, ... }:

let 
    emacsConfig = lib.readFile ./emacs.el;
in
{
    programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraConfig = emacsConfig;
    extraPackages = epkgs: [
        epkgs.jetbrains-darcula-theme
        epkgs.obsidian-theme
        epkgs.oblivion-theme
        epkgs.bongo

	epkgs.auto-complete
        epkgs.vlc
        epkgs.elcord
        epkgs.fsharp-mode
    ];
  };
}