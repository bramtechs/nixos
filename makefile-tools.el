;; Windows

(defun run-script (script-name)
  "Run the specified SCRIPT-NAME (either 'build' or 'run') if found in a dominating directory."
  (interactive)
  (let ((ps-script (locate-dominating-file default-directory (concat script-name ".ps1")))
        (cmd-script (locate-dominating-file default-directory (concat script-name ".cmd"))))
    (cond
     (ps-script
      (let ((default-directory ps-script))
        (compile (concat "powershell.exe -File " script-name ".ps1"))))
     (cmd-script
      (let ((default-directory cmd-script))
        (compile (concat script-name ".cmd"))))
     (t (message "No %s script found" script-name)))))

(defun run-build-script ()
  "Run the build script if found in a dominating directory."
  (interactive)
  (run-script "build"))

(defun run-run-script ()
  "Run the run script if found in a dominating directory."
  (interactive)
  (run-script "run"))

;; Linux / Mac

(defun run-makefile (&optional task)
  "Run the closest Makefile found from the current directory."
  (interactive)
  (setq makefile-folder (locate-dominating-file default-directory "Makefile"))
  (if makefile-folder
      (progn
        (setq makefile (concat makefile-folder "Makefile"))
        (compile (concat "make -f " makefile " -C " makefile-folder " -b " task))
        (message "Makefile %s is being run." makefile))
    (message "No Makefile found.")))

;; Shared

(defun build-project ()
  (setq compilation-scroll-output 'first-error) ;; stop compilation scroll on first error
  (if (eq system-type 'windows-nt)
      (run-build-script)
    (run-makefile)))

(defun run-project ()
  (setq compilation-scroll-output 't) ;; auto scroll compilation buffer
  (if (eq system-type 'windows-nt)
      (run-run-script)
    (run-makefile "run")))

;;;###autoload
(define-minor-mode slow-mode
  "Toggle slow-mode, which adds cooldowns to builds."
  :init-value nil
  :lighter (:eval (propertize " Slow" 'face '(:foreground "OrangeRed3")))
  :keymap (make-sparse-keymap)
  :global t
  (if slow-mode
      (message "Slow mode enabled")
    (message "Slow mode disabled")))

(provide 'slow-mode)

(defvar last-build-time -1)

(defcustom build-cooldown 300
  "Cooldown time (in seconds) between build commands."
  :type 'integer
  :group 'my-custom-settings)

(defun unix-time ()
    (time-convert (current-time) 'integer))

(defun cooldowned-action (cb)
  (setq time-left (- (+ last-build-time build-cooldown) (unix-time)))
  (if (or (<= time-left 0) (= last-build-time -1) (not slow-mode))
      (progn
        (funcall cb)
        (setq last-build-time (unix-time)))
      (message "Wait %d seconds for next build..." time-left)))

(defun build-project-a ()
  (cooldowned-action 'build-project))

(defun run-project-a ()
  (cooldowned-action 'run-project))

(global-set-key (kbd "<f5>") (lambda () (interactive) (build-project-a)))
(global-set-key (kbd "<f6>") (lambda () (interactive) (run-project-a)))
(global-set-key (kbd "<f7>") 'compile)
