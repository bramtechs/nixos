(setq package-list '(
		     exwm
	             auto-complete
  	             nix-mode
                     aggressive-indent
                     autothemer
                     clang-format
                     cmake-mode
                     csv-mode
                     d-mode
                     docker-compose-mode
                     dockerfile-mode
                     editorconfig
                     elcord
                     exec-path-from-shell
                     fsharp-mode
                     gcmh
                     git
                     github-dark-vscode-theme
                     groovy-mode
                     hl-todo
                     jetbrains-darcula-theme
                     jsonrpc
                     koopa-mode
                     kotlin-mode
                     lsp-mode
                     lua-mode
                     magit
                     make-color
                     markdown-mode
                     markdown-preview-mode
                     multiple-cursors
                     oblivion-theme
                     obsidian-theme
                     pdf-tools
                     php-mode
                     prettier
                     python-mode
                     quelpa
                     rainbow-mode
                     shader-mode
                     typescript-mode
                     vue-mode
                     yaml
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
