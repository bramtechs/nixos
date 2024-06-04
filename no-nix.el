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
                     koopa-mode
                     
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

;; map copilot
(setq warning-minimum-level :error) ;; hide annoying identation warnings
(global-set-key (kbd "C-x <tab>") 'copilot-accept-completion)
(global-set-key (kbd "C-x RET") 'copilot-accept-completion)

(require 'koopa-mode)

;; enable copilot for all modes except c and c++
;; (add-hook 'after-init-hook 'global-copilot-mode)
;; (add-hook 'c-mode-common-hook 'copilot-mode)
