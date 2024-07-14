
(defun mit-license ()
  (interactive)
  (goto-char (point-min))
  (insert "MIT License\n\n")
  (insert "Copyright (c) " (format-time-string "%Y") ". Doomhowl Interactive - bramtechs/brambasiel\n\n")
  (insert "Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE."))

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
