;; custom-emacs-theme.el --- Heavily hacked Gruber Darker color theme that
;; does not look like the Gruber Darker theme anymore. It now looks more
;; like gruvbox theme, except it has less colors.

;; Copyright (C) 2024 bramtechs a.k.a brambasiel
;; Copyright (C) 2013-2016 Alexey Kutepov a.k.a rexim
;; Copyright (C) 2009-2010 Jason R. Blevins

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Gruber Darker color theme for Emacs by Jason Blevins. A darker
;; variant of the Gruber Dark theme for BBEdit by John Gruber. Adapted
;; for deftheme and extended by Alexey Kutepov a.k.a. rexim. Adapted
;; again by bramtechs a.k.a brambasiel.

(deftheme custom-emacs
  "My custom theme for Emacs")

;; Please, install rainbow-mode.
;; Colors with +x are lighter. Colors with -x are darker.
(defconst main-color "#dbce9a")
(defconst bg-color "#192333")
(defconst bg-color-d "#0a0d12")

(let ((custom-emacs-fg        main-color)
      (custom-emacs-fg+1      main-color)
      (custom-emacs-fg+2      main-color)
      (custom-emacs-white     "#ebe5ce")
      (custom-emacs-black     "#000000")
      (custom-emacs-bg-1      bg-color-d)
      (custom-emacs-bg        bg-color)
      (custom-emacs-bg+4      "#aee4e8")
      (custom-emacs-bg+2      bg-color-d)
      (custom-emacs-bg+1      bg-color-d)
      (custom-emacs-bg+3      "#440044")
      (custom-emacs-red-1     "#c73c3f")
      (custom-emacs-red       "#f43841")
      (custom-emacs-red+1     "#ff4f58")
      (custom-emacs-brown     "#5ba651")
      (custom-emacs-yellow    "#ebe5ce")
      (custom-emacs-green     "#73c936") 
      (custom-emacs-quartz    "#aee4e8")
      (custom-emacs-niagara-2 main-color)
      (custom-emacs-niagara-1 main-color)
      (custom-emacs-niagara   main-color)
      (custom-emacs-wisteria  main-color))
  (custom-theme-set-variables
   'custom-emacs
   '(frame-brackground-mode (quote dark)))

  (custom-theme-set-faces
   'custom-emacs

   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,custom-emacs-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,custom-emacs-quartz))))
   `(agda2-highlight-function-face ((t (:foreground ,custom-emacs-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground custom-emacs-yellow
                                             :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,custom-emacs-green))))
   `(agda2-highlight-number-face ((t (:foreground ,custom-emacs-wisteria))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,custom-emacs-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,custom-emacs-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,custom-emacs-green))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground custom-emacs-niagara
                                             :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,custom-emacs-niagara))))
   `(font-latex-string-face ((t (:foreground ,custom-emacs-green))))
   `(font-latex-warning-face ((t (:foreground ,custom-emacs-red))))

   ;; Basic Coloring (or Uncategorized)
   `(border ((t ,(list :background custom-emacs-bg-1
                       :foreground custom-emacs-bg+2))))
   `(cursor ((t (:background ,custom-emacs-yellow))))
   `(default ((t ,(list :foreground custom-emacs-fg
                        :background custom-emacs-bg))))
   `(fringe ((t ,(list :background nil
                       :foreground custom-emacs-bg+2))))
   `(vertical-border ((t ,(list :foreground custom-emacs-bg+2))))
   `(link ((t (:foreground ,custom-emacs-niagara :underline t))))
   `(link-visited ((t (:foreground ,custom-emacs-wisteria :underline t))))
   `(match ((t (:background ,custom-emacs-bg+4))))
   `(shadow ((t (:foreground ,custom-emacs-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,custom-emacs-niagara))))
   `(region ((t (:background ,custom-emacs-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background custom-emacs-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground custom-emacs-black
                                    :background custom-emacs-red))))
   `(tooltip ((t ,(list :background custom-emacs-bg+4
                        :foreground custom-emacs-white))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,custom-emacs-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground custom-emacs-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground custom-emacs-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,custom-emacs-red+1))))
   `(compilation-mode-line-fail ((t ,(list :foreground custom-emacs-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground custom-emacs-green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; Completion
   `(completions-annotations ((t (:inherit 'shadow))))

   ;; Custom
   `(custom-state ((t (:foreground ,custom-emacs-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground custom-emacs-red+1
                             :background nil))))
   `(diff-added ((t ,(list :foreground custom-emacs-green
                           :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,custom-emacs-niagara :weight bold))))
   `(dired-ignored ((t ,(list :foreground custom-emacs-quartz
                              :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,custom-emacs-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,custom-emacs-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,custom-emacs-yellow))))
   `(egg-branch-mono ((t (:foreground ,custom-emacs-yellow))))
   `(egg-diff-add ((t (:foreground ,custom-emacs-green))))
   `(egg-diff-del ((t (:foreground ,custom-emacs-red))))
   `(egg-diff-file-header ((t (:foreground ,custom-emacs-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,custom-emacs-yellow))))
   `(egg-help-header-2 ((t (:foreground ,custom-emacs-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,custom-emacs-fg)))))
   `(egg-reflog-mono ((t (:foreground ,custom-emacs-niagara-1))))
   `(egg-section-title ((t (:foreground ,custom-emacs-yellow))))
   `(egg-text-base ((t (:foreground ,custom-emacs-fg))))
   `(egg-term ((t (:foreground ,custom-emacs-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,custom-emacs-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,custom-emacs-green))))
   `(erc-input-face ((t (:foreground ,custom-emacs-red+1))))
   `(erc-my-nick-face ((t (:foreground ,custom-emacs-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,custom-emacs-quartz))))
   `(eshell-ls-directory ((t (:foreground ,custom-emacs-niagara))))
   `(eshell-ls-executable ((t (:foreground ,custom-emacs-green))))
   `(eshell-ls-symlink ((t (:foreground ,custom-emacs-yellow))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,custom-emacs-yellow))))
   `(font-lock-comment-face ((t (:foreground ,custom-emacs-brown))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,custom-emacs-brown))))
   `(font-lock-constant-face ((t (:foreground ,custom-emacs-quartz))))
   `(font-lock-doc-face ((t (:foreground ,custom-emacs-green))))
   `(font-lock-doc-string-face ((t (:foreground ,custom-emacs-green))))
   `(font-lock-function-name-face ((t (:foreground ,custom-emacs-niagara))))
   `(font-lock-keyword-face ((t (:foreground ,custom-emacs-yellow :bold t))))
   `(font-lock-preprocessor-face ((t (:foreground ,custom-emacs-quartz))))
   `(font-lock-reference-face ((t (:foreground ,custom-emacs-quartz))))
   `(font-lock-string-face ((t (:foreground ,custom-emacs-green))))
   `(font-lock-type-face ((t (:foreground ,custom-emacs-quartz))))
   `(font-lock-variable-name-face ((t (:foreground ,custom-emacs-fg+1))))
   `(font-lock-warning-face ((t (:foreground ,custom-emacs-red))))

   ;; Flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,custom-emacs-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,custom-emacs-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,custom-emacs-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,custom-emacs-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,custom-emacs-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,custom-emacs-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,custom-emacs-red) :inherit unspecified))
      (t (:foreground ,custom-emacs-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,custom-emacs-yellow) :inherit unspecified))
      (t (:foreground ,custom-emacs-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background custom-emacs-bg+2
                                      :foreground custom-emacs-yellow
                                      :bold t))))
   `(helm-ff-directory ((t ,(list :foreground custom-emacs-niagara
                                  :background custom-emacs-bg
                                  :bold t))))
   `(helm-ff-executable ((t (:foreground ,custom-emacs-green))))
   `(helm-ff-file ((t (:foreground ,custom-emacs-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground custom-emacs-bg
                                        :background custom-emacs-red))))
   `(helm-ff-symlink ((t (:foreground ,custom-emacs-yellow :bold t))))
   `(helm-selection-line ((t (:background ,custom-emacs-bg+1))))
   `(helm-selection ((t (:background ,custom-emacs-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground custom-emacs-yellow
                                   :background custom-emacs-bg
                                   :box (list :line-width -1
                                              :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,custom-emacs-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,custom-emacs-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,custom-emacs-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,custom-emacs-niagara))))
   `(info-visited ((t (:foreground ,custom-emacs-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground custom-emacs-quartz
                                           :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,custom-emacs-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,custom-emacs-green))))
   `(jabber-rare-time-face ((t (:foreground ,custom-emacs-green))))
   `(jabber-roster-user-online ((t (:foreground ,custom-emacs-green))))
   `(jabber-activity-face ((t (:foreground ,custom-emacs-red))))
   `(jabber-activity-personal-face ((t (:foreground ,custom-emacs-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,custom-emacs-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background custom-emacs-bg+1
                                            :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,custom-emacs-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,custom-emacs-yellow))))

   ;; Linum
   `(linum ((t `(list :foreground custom-emacs-quartz
                      :background custom-emacs-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,custom-emacs-niagara))))
   `(magit-diff-hunk-header ((t (:background ,custom-emacs-bg+2))))
   `(magit-diff-file-header ((t (:background ,custom-emacs-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,custom-emacs-red+1))))
   `(magit-log-author ((t (:foreground ,custom-emacs-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground custom-emacs-green
                                            :background custom-emacs-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground custom-emacs-niagara
                                           :background custom-emacs-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground custom-emacs-yellow
                                          :background custom-emacs-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground custom-emacs-fg
                                          :background custom-emacs-bg+1))))
   `(magit-item-highlight ((t (:background ,custom-emacs-bg+1))))
   `(magit-tag ((t ,(list :foreground custom-emacs-yellow
                          :background custom-emacs-bg))))
   `(magit-blame-heading ((t ,(list :background custom-emacs-bg+1
                                    :foreground custom-emacs-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,custom-emacs-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background custom-emacs-bg+1
                          :foreground custom-emacs-white))))
   `(mode-line-buffer-id ((t ,(list :background custom-emacs-bg+1
                                    :foreground custom-emacs-white))))
   `(mode-line-inactive ((t ,(list :background custom-emacs-bg+1
                                   :foreground custom-emacs-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,custom-emacs-niagara))))

   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,custom-emacs-niagara))))
   `(org-column ((t (:background ,custom-emacs-bg-1))))
   `(org-column-title ((t (:background ,custom-emacs-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,custom-emacs-green))))
   `(org-todo ((t (:foreground ,custom-emacs-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,custom-emacs-yellow))))

   ;; Search
   `(isearch ((t ,(list :foreground custom-emacs-black
                        :background custom-emacs-fg+2))))
   `(isearch-fail ((t ,(list :foreground custom-emacs-black
                             :background custom-emacs-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground custom-emacs-fg+1
                                       :background custom-emacs-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,custom-emacs-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,custom-emacs-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,custom-emacs-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,custom-emacs-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,custom-emacs-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground custom-emacs-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,custom-emacs-fg))))
   `(speedbar-highlight-face ((t (:background ,custom-emacs-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,custom-emacs-red))))
   `(speedbar-tag-face ((t (:foreground ,custom-emacs-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,custom-emacs-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background custom-emacs-bg
                                 :foreground custom-emacs-bg+1))))
   `(whitespace-tab ((t ,(list :background custom-emacs-bg
                               :foreground custom-emacs-bg+1))))
   `(whitespace-hspace ((t ,(list :background custom-emacs-bg
                                  :foreground custom-emacs-bg+2))))
   `(whitespace-line ((t ,(list :background custom-emacs-bg+2
                                :foreground custom-emacs-red+1))))
   `(whitespace-newline ((t ,(list :background custom-emacs-bg
                                   :foreground custom-emacs-bg+2))))
   `(whitespace-trailing ((t ,(list :background custom-emacs-red
                                    :foreground custom-emacs-red))))
   `(whitespace-empty ((t ,(list :background custom-emacs-yellow
                                 :foreground custom-emacs-yellow))))
   `(whitespace-indentation ((t ,(list :background custom-emacs-yellow
                                       :foreground custom-emacs-red))))
   `(whitespace-space-after-tab ((t ,(list :background custom-emacs-yellow
                                           :foreground custom-emacs-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background custom-emacs-brown
                                            :foreground custom-emacs-brown))))

   ;; tab-bar
   `(tab-bar ((t (:background ,custom-emacs-bg+1 :foreground ,custom-emacs-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,custom-emacs-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,custom-emacs-bg+3 :background ,custom-emacs-bg+4))))
   `(term-color-red ((t (:foreground ,custom-emacs-red-1 :background ,custom-emacs-red-1))))
   `(term-color-green ((t (:foreground ,custom-emacs-green :background ,custom-emacs-green))))
   `(term-color-blue ((t (:foreground ,custom-emacs-niagara :background ,custom-emacs-niagara))))
   `(term-color-yellow ((t (:foreground ,custom-emacs-yellow :background ,custom-emacs-yellow))))
   `(term-color-magenta ((t (:foreground ,custom-emacs-wisteria :background ,custom-emacs-wisteria))))
   `(term-color-cyan ((t (:foreground ,custom-emacs-quartz :background ,custom-emacs-quartz))))
   `(term-color-white ((t (:foreground ,custom-emacs-fg :background ,custom-emacs-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,custom-emacs-fg :background ,custom-emacs-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,custom-emacs-brown :background ,custom-emacs-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,custom-emacs-brown :background ,custom-emacs-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,custom-emacs-fg :background ,custom-emacs-bg-1))))
   `(company-tooltip-mouse ((t (:background ,custom-emacs-bg-1))))
   `(company-tooltip-common ((t (:foreground ,custom-emacs-green))))
   `(company-tooltip-common-selection ((t (:foreground ,custom-emacs-green))))
   `(company-scrollbar-fg ((t (:background ,custom-emacs-bg-1))))
   `(company-scrollbar-bg ((t (:background ,custom-emacs-bg+2))))
   `(company-preview ((t (:background ,custom-emacs-green))))
   `(company-preview-common ((t (:foreground ,custom-emacs-green :background ,custom-emacs-bg-1))))

   ;; Proof General
   `(proof-locked-face ((t (:background ,custom-emacs-niagara-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,custom-emacs-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,custom-emacs-green))))
   `(orderless-match-face-2 ((t (:foreground ,custom-emacs-brown))))
   `(orderless-match-face-3 ((t (:foreground ,custom-emacs-quartz))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'custom-emacs)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; custom-emacs-theme.el ends here.
