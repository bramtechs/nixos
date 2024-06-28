
(defun add-copyright ()
  (interactive)
  (goto-char (point-min))
  (insert "/*\n")
  (insert " * Copyright (c) " (format-time-string "%Y") ". Doomhowl Interactive - All Rights Reserved\n")
  (insert " * Redistribution and use in source and binary forms, with or without modification are not permitted\n")
  (insert " * without the prior written permission of Doomhowl Interactive.\n")
  (insert " *\n")
  (insert " * File created on: " (format-time-string "%d-%m-%Y") "\n")
  (insert " */\n\n"))

(defun my-cc-mode-setup ()
  "Custom setup for C/C++ files and headers."
  (when (and (buffer-file-name)
             (not (file-exists-p (buffer-file-name)))
             (or (string-match "\\.\\(c\\|cpp\\|h\\|hpp\\)\\'" (buffer-file-name))))
    ;; Insert a header if the file is new
    (add-copyright)
    
    ;; Move the cursor to the end of the header
    (goto-char (point-max))
    (message "Added license text for this file")))

;; Add the function to C and C++ mode hooks
(add-hook 'c-mode-hook 'my-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-cc-mode-setup)
