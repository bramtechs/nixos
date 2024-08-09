{ config, lib, pkgs, callPackage, ... }:

let
  emacsConfig = lib.readFile ./emacs.el;
  unstable = import
    (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/b3aa3989b4d23f53153603262b1756e06def9c64")
    { config = config.nixpkgs.config; };
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
      epkgs.yaml-mode
      
      epkgs.hl-todo
      epkgs.aggressive-indent
	    epkgs.auto-complete
      epkgs.markdown-preview-mode
      epkgs.git
      epkgs.rainbow-mode
      epkgs.pdf-tools
      epkgs.multiple-cursors
      epkgs.emojify
      epkgs.magit
      epkgs.exwm
      unstable.emacsPackages.copilot
      
      # distractions
      epkgs.bongo
      epkgs.vlc
      epkgs.elcord
      epkgs.playerctl
      epkgs.ement
      epkgs.ivy
      epkgs.ivy-youtube
      epkgs.fireplace
    ];
  };
}
