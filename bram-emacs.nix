{ config, lib, pkgs, callPackage, ... }:

let
  emacsConfig = lib.readFile ./emacs.el;
in
{
  programs.emacs = {
    enable = true;
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
      epkgs.groovy-mode
      epkgs.gcmh
      epkgs.hl-todo
      epkgs.aggressive-indent
      epkgs.auto-complete
      epkgs.markdown-preview-mode
      epkgs.git
      epkgs.rainbow-mode
      epkgs.pdf-tools
      epkgs.multiple-cursors
      epkgs.magit
      epkgs.copilot
      epkgs.kotlin-mode

      # distractions
      epkgs.elcord
      epkgs.playerctl
    ];
  };
}
