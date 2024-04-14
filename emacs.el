;; Hide bloat
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(if window-system
    (tool-bar-mode -1)
)

;; Tab size is 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; nowrap
(set-default 'truncate-lines t)
    
(set-frame-font "Ubuntu Mono 17" nil t)

;; Ctrl x,c,v
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

(ac-config-default)


(load-theme 'gruber-darker t)

;; c-style language formatting
(setq c-default-style
      '((c++-mode . "stroustrup")
        (java-mode . "java")))

;; keybindings
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f2>") (lambda () (interactive) (find-file "~/dev/nixos/emacs.el")))

;; elcord (larp-mode)
(elcord-mode)
(setq elcord-display-elapsed 'f)
(setq elcord-quiet 't)
(setq elcord-refresh-rate 7)
(setq elcord-idle-message "Howling at the moon.")
(defun elcord--editor-icon ()
  "The icon to use to represent the current editor."
  "https://raw.githubusercontent.com/bramtechs/nixos-config/main/misc/icon-invert-skew.png")
