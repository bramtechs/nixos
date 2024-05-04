{ config, lib, pkgs, callPackage, ... }:

let
    emacsConfig = lib.readFile ./emacs.el;
in
{
    programs.emacs = {
    enable = true;
    package = pkgs.emacs29-gtk3;
    
    extraConfig = emacsConfig;

    extraPackages = epkgs: [
      
      # themes
      epkgs.jetbrains-darcula-theme
      epkgs.obsidian-theme
      epkgs.oblivion-theme
      
      # language modes
      epkgs.janet-mode
      epkgs.lua-mode
      epkgs.d-mode
      epkgs.make-color
      epkgs.fsharp-mode
  	  epkgs.nix-mode
      epkgs.markdown-mode
      epkgs.shader-mode
      epkgs.shader-mode
      epkgs.python-mode
      epkgs.php-mode
      epkgs.typescript-mode
      epkgs.vue-mode
      epkgs.csv-mode
      epkgs.cmake-mode
     
	    epkgs.auto-complete
      epkgs.markdown-preview-mode
      epkgs.git
      epkgs.rainbow-mode
      epkgs.pdf-tools
      epkgs.multiple-cursors
      
      # distractions
      epkgs.bongo
      epkgs.vlc
      epkgs.elcord
      epkgs.playerctl
      epkgs.ement
      epkgs.ivy
      epkgs.ivy-youtube
    ];
  };
}
