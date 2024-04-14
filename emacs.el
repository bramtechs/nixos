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

;; markdown preview
(custom-set-variables
 '(markdown-command "pandoc"))

;; autocomplete
(ac-config-default)

;; pdf support
(pdf-tools-install)

;; set theme
(load-theme 'gruber-darker t)

;; c-style language formatting
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "stroustrup")
        (java-mode . "java")))

;; compiling
(defun find-closest-makefile ()
  "Find the closest Makefile starting from the current directory."
  (let ((dir (locate-dominating-file default-directory "Makefile")))
    (if dir
        (concat (file-name-as-directory dir) "Makefile")
      nil)))

(defun find-closest-makefile-folder ()
  "Find the closest Makefile starting from the current directory."
  (locate-dominating-file default-directory "Makefile"))

(defun run-makefile (&optional task)
  "Run the closest Makefile found from the current directory."
  (interactive)
  (let ((makefile (find-closest-makefile)))
    (if makefile
        (progn
          (compile (concat "make -f " makefile " -C " (find-closest-makefile-folder) " -b " task))
          (message "Makefile %s is being run." makefile))
      (message "No Makefile found."))))

(defun build-project ()
  (setq compilation-scroll-output 'first-error) ;; stop compilation scroll on first error
  (run-makefile))

(defun run-project ()
  (setq compilation-scroll-output 't) ;; auto scroll compilation buffer
  (run-makefile "run"))

(defun edit-config ()
  (find-file "~/dev/nixos/emacs.el"))

(defun edit-nix-config ()
  (find-file "~/dev/nixos/bram-emacs.nix"))

;; keybindings
(global-set-key (kbd "<f2>") (lambda () (interactive) (edit-config)))
(global-set-key (kbd "S-<f2>") (lambda () (interactive) (edit-nix-config)))
(global-set-key (kbd "<f5>") (lambda () (interactive) (build-project)))
(global-set-key (kbd "<f6>") (lambda () (interactive) (run-project)))
(global-set-key (kbd "<f7>") 'compile)

;; map zoom to sane bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; elcord (larp-mode)
(elcord-mode)
(setq elcord-icon-base '"https://raw.githubusercontent.com/bramtechs/elcord/master/icons/")
(setq elcord-mode-icon-alist (append elcord-mode-icon-alist '((janet-mode . "janet-mode_icon"))))

(setq elcord-display-elapsed 'f)
(setq elcord-quiet 't)
(setq elcord-refresh-rate 7)
(setq elcord-idle-message "Howling at the moon...")
(defun elcord--editor-icon ()
  "The icon to use to represent the current editor."
  "https://raw.githubusercontent.com/bramtechs/nixos-config/main/misc/icon-invert-skew.png")
