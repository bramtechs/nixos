{ lib, pkgs, ... }:

let
  nvimConfig = lib.readFile ./nvim.vim;
  fromGitHub = ref: repo: pkgs.vimUtils.buildVimPlugin {
      pname = "${lib.strings.sanitizeDerivationName repo}";
      version = ref;
      src = builtins.fetchGit {
        url = "https://github.com/${repo}.git";
        ref = ref;
      };
  };
in
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; with pkgs.vimPlugins.nvim-treesitter-parsers; [
      telescope-nvim
      gruvbox-nvim
      vim-auto-save
      nvim-treesitter.withAllGrammars
      (fromGitHub "HEAD" "janet-lang/janet.vim")
    ];
    viAlias = true;
    vimAlias = true;

    extraConfig = nvimConfig;
  };
}
