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
      
      # themes
      epkgs.jetbrains-darcula-theme
      epkgs.obsidian-theme
      epkgs.oblivion-theme
      epkgs.gruber-darker-theme

      # language modes
      epkgs.janet-mode
      epkgs.fsharp-mode      
  	  epkgs.nix-mode
      epkgs.markdown-mode

      # utils
	    epkgs.auto-complete
      epkgs.markdown-preview-mode
      epkgs.git
      epkgs.rainbow-mode

      # distractions
      epkgs.bongo
      epkgs.vlc
      epkgs.elcord
      epkgs.playerctl
    ];
  };
}
