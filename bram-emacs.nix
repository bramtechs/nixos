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
        epkgs.janet-mode
        epkgs.git
  	    epkgs.nix-mode
	      epkgs.auto-complete
        epkgs.vlc
        epkgs.elcord
        epkgs.gruber-darker-theme
        epkgs.fsharp-mode
    ];
  };
}
