(custom-set-variables
 '(inhibit-startup-message t)
 '(inhibit-splash-screen t)
 '(initial-scratch-message nil))

(menu-bar-mode -1)
(tool-bar-mode -1)

(if (eq system-type 'windows-nt)
    (setq no-nix t))

;; fix grep on windows
(when (eq system-type 'windows-nt)
  (with-eval-after-load 'grep
    ;; findstr can handle the basic find|grep use case
    (grep-apply-setting 'grep-find-template
                        "findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil))

  ;; register reload config cmd
  (defun reload-config ()
    "Reload Emacs config"
    (interactive)
    (load-file "~/.emacs")))

;; install packages manually when not using nix
(if (bound-and-true-p no-nix)
    (load-file "no-nix.el")
  ;; migrate to nixos config folder
  (cd "~/dev/nixos"))

;; make emacs shut up
(setq ring-bell-function 'ignore)
(setq set-message-beep 'silent)

;; Tab size is 4 spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-line-function 'insert-tab)

;; nowrap
(set-default 'truncate-lines t)

(cond
 ((eq system-type 'darwin)
  (set-frame-font "Monaco 17" nil t))
 ((eq system-type 'windows-nt)
  (set-frame-font "Cascadia Code 14" nil t))
 (t
  (set-frame-font "Ubuntu Mono 12" nil t)))

;; reduce some friction
(setq use-short-answers t)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; vscode-like file opening
(global-set-key (kbd "C-x p") 'project-find-file)

;; Ctrl x,c,v :: Conflicts with emacs sometimes, but old habits don't die.
;; Workarounds:
;; - Press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds, or
;; - use the SHIFT key with the prefix key, i.e. C-S-x or C-S-c.
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

;; multi cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; window swapping
(global-set-key (kbd "M-s-<right>") 'windmove-swap-states-right)
(global-set-key (kbd "M-s-<left>") 'windmove-swap-states-left)
(global-set-key (kbd "M-s-<up>") 'windmove-swap-states-up)
(global-set-key (kbd "M-s-<down>") 'windmove-swap-states-down)

;; move between windows
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; markdown preview
(custom-set-variables
 '(markdown-command "pandoc"))

;; autocomplete
;;(ac-config-default)

;;(global-aggressive-indent-mode 1)
(global-hl-todo-mode)

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
(add-to-list 'custom-theme-load-path "/mnt/c/dev/nixos/") ;; wsl
(add-to-list 'custom-theme-load-path "C:/dev/nixos/") ;; windows

(load-theme 'custom-emacs t)

;; c-style language formatting
(setq c-default-style
      '((c++-mode . "stroustrup")
        (c-mode . "stroustrup")
        (java-mode . "java")))

;; Sign source code files. If you're not me you'll probably want to
;; get rid of this or change it's contents.
(load-file "copyright.el")

;; etags
(setq tags-revert-without-query 1)

;; Support colors in compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun edit-config ()
  (find-file
   (cond
    ((eq system-type 'windows-nt) "C:/dev/nixos/emacs.el")
    ((file-exists-p "~/dev/nixos/emacs.el") "~/dev/nixos/emacs.el")
    (t "/mnt/c/dev/nixos/emacs.el"))))

(defun edit-nix-config ()
  (if (eq system-type 'windows-nt)
      (message "Not using Nix")
    (find-file
     (if (file-exists-p "~/dev/nixos/bram-emacs.nix")
         "~/dev/nixos/bram-emacs.nix"
       "/mnt/c/dev/nixos/bram-emacs.nix"))))

;; auto-revert-mode might be slow, so try to disable it
(with-eval-after-load 'magit-autorevert
  (defalias 'magit-auto-revert-buffers 'auto-revert-buffers)
  (magit-auto-revert-mode -1))
    
;; keybindings
(global-set-key (kbd "<f2>") (lambda () (interactive) (edit-config)))
(global-set-key (kbd "S-<f2>") (lambda () (interactive) (edit-nix-config)))

;; map zoom to sane bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; interact with discord through irc-client
(global-set-key (kbd "C-c rc") (lambda () (interactive)
                           (erc :server "localhost" :port "6667"
                                :nick "brambasiel")))
;; eww
(cond
 ((eq system-type 'windows-nt)
  (load-file "C:/dev/nixos/epithet.el"))
 ((file-exists-p "~/dev/nixos/epithet.el")
  (load-file "~/dev/nixos/epithet.el"))
 (t
  (load-file "/mnt/c/dev/nixos/epithet.el"))) ;; wsl

(add-hook 'eww-after-render-hook #'epithet-rename-buffer)
;;(setq eww-retrieve-command '("google-chrome-stable" "--headless" "--dump-dom"))

;; disable MMM background
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

;; shorthands
(defun mwb ()
  (interactive)
  (mark-whole-buffer))
(defun mhb ()
  (interactive)
  (mark-whole-buffer))

;; load credentials
(defconst creds-file "~/.env.el")
(if (file-readable-p creds-file)
    (progn
      (load-file creds-file)
      (defun erc-discord ()
        (interactive)
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
        (message "Signed into Discord!")))
    (message "Could not find credential file, at" creds-file))
  
;; ivy
(setq ivy-youtube-play-at "mpv")

;; map copilot
(require 'copilot)
(setq warning-minimum-level :error) ;; hide annoying identation warnings
(global-set-key (kbd "C-x <tab>") 'copilot-accept-completion)
(global-set-key (kbd "C-x RET") 'copilot-accept-completion)

;; elcord (larp-mode)
(elcord-mode)
(setq elcord-icon-base '"https://raw.githubusercontent.com/bramtechs/elcord/own/icons/")
(setq elcord-mode-icon-alist (append elcord-mode-icon-alist
                                     '((janet-mode . "janet-mode_icon")
                                       (asm-mode . "assembly-mode_icon")
                                       (nasm-mode . "assembly-mode_icon")
                                       (d-mode . "d-mode_icon"))))

(setq elcord-display-elapsed nil)
(setq elcord-quiet 't)
(setq elcord-refresh-rate 12)
(setq elcord-idle-message "Howling at the moon...")

(defun elcord--editor-icon ()
  "The icon to use to represent the current editor."
  "https://raw.githubusercontent.com/bramtechs/nixos-config/main/misc/icon-invert-skew.png")


;; stop fat-fingering suspend shortcut
(global-unset-key "\C-x\C-z")
(put 'suspend-frame 'disabled t)

;; run exwm environment when on nixos
(when (not (bound-and-true-p no-nix))
  
  (defun quit-x ()
    (interactive)
    (start-process-shell-command "pkill" nil "pkill -n X")))

;; build and run keys
(load-file "makefile-tools.el")

(message "Loaded entire config successfully")
