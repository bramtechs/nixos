
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(load-theme "jetbrains-darcula")
(set-frame-font "Ubuntu Mono 17" nil t)

;; Ctrl x,c,v
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1)               ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) 

(ac-config-default)
