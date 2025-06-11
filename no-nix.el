(setq package-list '(
                     jetbrains-darcula-theme
                     obsidian-theme
                     oblivion-theme
                     github-dark-vscode-theme
                     autothemer
                     lua-mode
                     d-mode
                     make-color
                     fsharp-mode
  	             nix-mode
                     markdown-mode
                     shader-mode
                     python-mode
                     php-mode
                     groovy-mode
                     typescript-mode
                     vue-mode
                     csv-mode
                     cmake-mode
                     pdf-tools
                     koopa-mode
                     kotlin-mode
                     dockerfile-mode
                     docker-compose-mode
                     yaml
                     magit
                     hl-todo
                     aggressive-indent
	             auto-complete
                     markdown-preview-mode
                     git
                     rainbow-mode
                     multiple-cursors
                     clang-format
                     lsp-mode
                     editorconfig
                     jsonrpc
                     quelpa
                     gcmh
                     elcord
                     prettier
                     exec-path-from-shell
                     ))

(setq package-user-dir "~/.emacs.d/packages")

;; register melpa repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ensure shell path is used on MacOS and such
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
