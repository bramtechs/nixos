
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
