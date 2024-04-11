{ config, lib, pkgs, callPackage, ... }:

let
  nvimConfig = lib.readFile ./nvim.vim;
in
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; with pkgs.vimPlugins.nvim-treesitter-parsers; [
      telescope-nvim
      gruvbox-nvim
      nvim-lspconfig
      mason-nvim
      mason-lspconfig-nvim

      lsp-zero-nvim
      vim-auto-save

      nvim-treesitter

      xml
      yaml
      zig
      vue
      vala
      v
      typescript
      tsx
      toml
      svelte
      cpp
      c
      csv
      css
      cmake
      c_sharp
      json
      javascript
      d
      dockerfile
      gdscript
      lua
      rust
      php
      odin
      objc
      java
      html
      glsl
      bash
      ocaml
      latex
      python
      pascal
      nix
      kotlin
      haskell
      groovy
      markdown
      janet_simple

    ];
    viAlias = true;
    vimAlias = true;

    extraConfig = nvimConfig;
  };
}
