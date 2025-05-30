(setq package-list '(
                     ;; themes
                     jetbrains-darcula-theme
                     obsidian-theme
                     oblivion-theme
                     github-dark-vscode-theme
                     autothemer

                     ;; language modes
                     janet-mode
                     lua-mode
                     d-mode
                     make-color
                     fsharp-mode
  	             nix-mode
                     markdown-mode
                     shader-mode
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

                     ;; copilot
                     editorconfig
                     jsonrpc
                     quelpa
                     gcmh

                     ;; distractions
                     bongo
                     vlc
                     elcord
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
