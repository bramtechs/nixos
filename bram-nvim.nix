{ config, lib, pkgs, callPackage, ... }:

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
      vimPlugins.vimsence

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

    extraConfig = ''
      set number relativenumber
      set number
      set nowrap
      set linebreak
      set ff=unix

      set relativenumber
      set smartindent
      set guicursor:
      set hidden
      set noerrorbells
      set smartcase
      set nu
      set scrolloff=8
      set nohlsearch
      set list

      set noswapfile
      set undodir=~/.vim/undodir
      set undofile
      set nobackup
      set nowritebackup

      set incsearch
      set ignorecase
      set cmdheight=1
      set laststatus=2

      set updatetime=300
      set shortmess+=c

      set mousemodel=popup

      set tabstop=4 softtabstop=4
      set shiftwidth=4
      set expandtab

      set updatetime=300
      set signcolumn=yes
      set spelllang=en_us,nl

      set autoread

      colorscheme gruvbox

      let mapleader = " "

      " allow traversal of wrapped lines
      noremap j gj
      noremap k gk

      nnoremap <S-F2> :tabedit ~/dev/monolith/nixos/configuration.nix<CR>
      nnoremap <leader><F2> :so $MYVIMRC<CR>

      nnoremap <F8> :tabedit ~/TODO.md<CR>
      nnoremap <A-j> :tabprevious<CR>
      nnoremap <A-k> :tabnext<CR>
      nnoremap <A-n> :tabnew<CR>

      tnoremap <leader><Esc> <C-\><C-n>

      " remove comments after a line
      nnoremap <S-E> 0f;lv$hx0

      " relocate neovim
      nnoremap <leader>R :lcd %:p:h<CR>:!echo Moved instance to %:p:h<CR>

      " actually use vlang filetype
      au BufRead,BufNewFile *.v   set filetype=vlang

      " threat HolyC as regular C (blasphomy)
      au BufRead, BufNewFile *.HC set filetype=c

      nnoremap T :terminal<CR>i
      nnoremap <leader>t :vsplit<CR><C-w>l<CR>:terminal<CR>i

      " latex shortcuts
      nnoremap <leader>l% i\mathbin{\%}<ESC>
      nnoremap <leader>ls i\section{
      nnoremap <leader>lS i\subsection{
      nnoremap <leader>ld i\begin{document}<ESC>
      nnoremap <leader>lD i\end{document}<ESC>
      nnoremap <leader>lb i\textbf{
      nnoremap <leader>lf i\[  \]<ESC>F[lli

      " change tabs to spaces
      nnoremap <leader>ts :%s/\t/    /g<CR>

      " auto save
      let g:auto_save = 1  " enable AutoSave on Vim startup
      let g:auto_save_no_updatetime = 1  " do not change the 'updatetime' option
      let g:auto_save_in_insert_mode = 0  " do not save while in insert mode
      let g:auto_save_silent = 1  " do not display the auto-save notification

      " telescope
      lua << EOF
          local builtin = require('telescope.builtin')
          vim.keymap.set('n', '<leader>ff', builtin.git_files, {})
          vim.keymap.set('n', '<leader>fF', builtin.find_files, {})
          vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
          vim.keymap.set('n', '<leader>fs', builtin.grep_string, {})
          vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
          vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

          require('telescope').setup({
            defaults = {
              ripgrep_arguments = {
                'rg',
                '--hidden',
                '--no-heading',
                '--with-filename',
                '--line-number',
                '--column',
                '--smart-case'
              },
            },
          })

          -- LSPs are so good (no, they're jank as hell)
          local lsp_zero = require('lsp-zero')

          lsp_zero.on_attach(function(client, bufnr)
            -- see :help lsp-zero-keybindings
            -- to learn the available actions
            lsp_zero.default_keymaps({buffer = bufnr})
          end)

          require("mason").setup()
          require("mason-lspconfig").setup {
              ensure_installed = { "lua_ls", "tsserver", "eslint", "clangd", "cssls", "jdtls" },
              automatic_installation = true,
              handlers = {
                  lsp_zero.default_setup,
              },
          }

      EOF
    '';

  };
}
