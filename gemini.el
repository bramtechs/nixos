(require 'json)
(require 'url)

(defvar gemini-api-key-file "~/.config/gemini-api-key"
  "Path to file containing your Google Gemini API key.")

(defun gemini--read-api-key ()
  "Read the Gemini API key from a local file."
  (with-temp-buffer
    (insert-file-contents gemini-api-key-file)
    (string-trim (buffer-string))))

(defun gemini--callback (status)
  "Handle Gemini response and show result in *gemini-result* buffer."
  (message "Got response")
  (goto-char url-http-end-of-headers)
  (condition-case err
      (let* ((json-response (json-parse-buffer :object-type 'hash-table))
             (candidates (gethash "candidates" json-response))
             (first-candidate (aref candidates 0))
             (content (gethash "content" first-candidate))
             (parts (gethash "parts" content))
             (text-part (aref parts 0))
             (text (gethash "text" text-part))
             (buffer (get-buffer-create "*gemini-result*")))
        ;; Display the result
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            (insert (or text "No response from Gemini."))
            (goto-char (point-min))))
        ;; Show buffer and switch to it
        (pop-to-buffer buffer))
    (error
     (message "Gemini error: %s" err)
     (kill-buffer (current-buffer)))))

;;;###autoload
(defun gemini (prompt)
  "Send PROMPT to Google Gemini and display the result in a separate buffer."
  (interactive "sGemini Prompt: ")
  (let* ((api-key (gemini--read-api-key))
         (api-url "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent")
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(("contents" .
              ,(vector (list (cons "parts"
                                  (vector (list (cons "text" (concat prompt " The result of this prompt will appear in an Emacs buffer. Write the result in emacs org-mode syntax without anything else."))))))))))))
    (url-retrieve (concat api-url "?key=" api-key)
                  #'gemini--callback)))
