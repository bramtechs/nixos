{ config, lib, pkgs, callPackage, ... }:

let
  nvimConfig = lib.readFile ./nvim.vim;
in
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs; [
      vimPlugins.telescope-nvim
      vimPlugins.gruvbox-nvim
      vimPlugins.nvim-lspconfig
      vimPlugins.mason-nvim
      vimPlugins.mason-lspconfig-nvim

      vimPlugins.lsp-zero-nvim
      vimPlugins.vim-auto-save

      # List of additional treesitter parsers (still not learning Nix language)
      vimPlugins.nvim-treesitter
      vimPlugins.nvim-treesitter-parsers.xml
      vimPlugins.nvim-treesitter-parsers.yaml
      vimPlugins.nvim-treesitter-parsers.zig
      vimPlugins.nvim-treesitter-parsers.vue
      vimPlugins.nvim-treesitter-parsers.vala
      vimPlugins.nvim-treesitter-parsers.v
      vimPlugins.nvim-treesitter-parsers.typescript
      vimPlugins.nvim-treesitter-parsers.tsx
      vimPlugins.nvim-treesitter-parsers.toml
      vimPlugins.nvim-treesitter-parsers.svelte
      vimPlugins.nvim-treesitter-parsers.cpp
      vimPlugins.nvim-treesitter-parsers.c
      vimPlugins.nvim-treesitter-parsers.csv
      vimPlugins.nvim-treesitter-parsers.css
      vimPlugins.nvim-treesitter-parsers.cmake
      vimPlugins.nvim-treesitter-parsers.c_sharp
      vimPlugins.nvim-treesitter-parsers.json
      vimPlugins.nvim-treesitter-parsers.javascript
      vimPlugins.nvim-treesitter-parsers.d
      vimPlugins.nvim-treesitter-parsers.dockerfile
      vimPlugins.nvim-treesitter-parsers.gdscript
      vimPlugins.nvim-treesitter-parsers.lua
      vimPlugins.nvim-treesitter-parsers.rust
      vimPlugins.nvim-treesitter-parsers.php
      vimPlugins.nvim-treesitter-parsers.odin
      vimPlugins.nvim-treesitter-parsers.objc
      vimPlugins.nvim-treesitter-parsers.java
      vimPlugins.nvim-treesitter-parsers.html
      vimPlugins.nvim-treesitter-parsers.glsl
      vimPlugins.nvim-treesitter-parsers.bash
      vimPlugins.nvim-treesitter-parsers.ocaml
      vimPlugins.nvim-treesitter-parsers.latex
      vimPlugins.nvim-treesitter-parsers.python
      vimPlugins.nvim-treesitter-parsers.pascal
      vimPlugins.nvim-treesitter-parsers.nix
      vimPlugins.nvim-treesitter-parsers.kotlin
      vimPlugins.nvim-treesitter-parsers.haskell
      vimPlugins.nvim-treesitter-parsers.groovy
      vimPlugins.nvim-treesitter-parsers.markdown
    ];
    viAlias = true;
    vimAlias = true;

    extraConfig = nvimConfig;
  };
}
