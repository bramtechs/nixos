(setq package-list '(
                     ;; themes
                     jetbrains-darcula-theme
                     obsidian-theme
                     oblivion-theme
                     
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
                     typescript-mode
                     vue-mode
                     csv-mode
                     cmake-mode
                     pdf-tools
                     
                     hl-todo
                     aggressive-indent
	             auto-complete
                     markdown-preview-mode
                     git
                     rainbow-mode
                     multiple-cursors

                     ;; copilot
                     editorconfig
                     jsonrpc
                     quelpa
                         
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

;; quelpa packages
(require 'quelpa)
(quelpa '(copilot :fetcher github
                  :repo "copilot-emacs/copilot.el"
                  :branch "main"
                  :files ("*.el")))
