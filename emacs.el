;; Hide the bloat (it stil doesn't go away wtf)
(custom-set-variables
 '(inhibit-startup-message t)
 '(inhibit-splash-screen t)
 '(initial-scratch-message nil))
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Tab size is 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; nowrap
(set-default 'truncate-lines t)
    
(set-frame-font "Ubuntu Mono 17" nil t)

;; Ctrl x,c,v :: Conflicts with emacs sometimes, but old habits don't die.
;; Workarounds:
;; - Press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds, or
;; - use the SHIFT key with the prefix key, i.e. C-S-x or C-S-c.
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

;; markdown preview
(custom-set-variables
 '(markdown-command "pandoc"))

;; autocomplete
;; (ac-config-default)

;; pdf support
(pdf-tools-install)

;; org mode
(setq org-support-shift-select 't)

;; auto open shader-mode
(add-to-list 'auto-mode-alist '("\\.vs\\'" . shader-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . shader-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . shader-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . shader-mode))

;; set theme
(add-to-list 'custom-theme-load-path "~/dev/nixos/")
(load-theme 'custom-emacs t)

;; c-style language formatting
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "stroustrup")
        (java-mode . "java")))

;; compiling

;; Support colors in compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

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

;; interact with discord through irc-client
(global-set-key (kbd "C-c rc") (lambda () (interactive)
                           (erc :server "localhost" :port "6667"
                                :nick "brambasiel")))



(defun erc-discord ()
  (interactive)
  (defvar creds-file "~/.env.el")

  (if (file-readable-p creds-file)
      (progn (load creds-file)
             (defun erc-cmd (c &optional hide)
               (interactive)
               (setq erc-accidental-paste-threshold-seconds 0)
               (insert c)
               (erc-send-current-line)
               (if hide
                   (progn (insert "/clear")
                          (erc-send-current-line))))
             
             (erc-cmd (concat "account add eionrobb-discord " dc-email " " dc-pwd) t)
             (erc-cmd "account 0 on")
             
             ;; Don't leave credentials in memory after logging in.
             (setq dc-email "")
             (setq dc-pwd "")
             (message "Signed into Discord!"))
    (message "Could not load Discord credentials, at" creds-file)))
  

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


