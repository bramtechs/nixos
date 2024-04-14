
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(set-frame-font "Ubuntu Mono 17" nil t)

;; Ctrl x,c,v
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

(ac-config-default)


(load-theme 'jetbrains-darcula t)

;; elcord (larp-mode)
(elcord-mode)
(setq elcord-display-elapsed 'f)
(setq elcord-quiet 't)
(setq elcord-refresh-rate 7)

(defun elcord--editor-icon ()
  "The icon to use to represent the current editor."
  "https://raw.githubusercontent.com/bramtechs/nixos-config/main/misc/icon-invert-skew.png")
