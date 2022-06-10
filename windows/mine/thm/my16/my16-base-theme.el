;;; Code:

(defun my16-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key       (car  spec))
             (value     (cadr spec))
             (color-key (if (symbolp value) (intern (concat ":" (symbol-name value))) nil))
             (color     (plist-get colors color-key)))

        ;; Append the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output (list key (my16-transform-spec value colors)))))
         (color
          (setq output (append output (list key color))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun my16-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face         (car spec))
         (definition   (cdr spec)))
    (list face `((((type graphic)) ,(my16-transform-spec definition colors))))))


(defun my16-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (my16-transform-face face colors))
                 faces)))

(defun my16-theme-define (theme-name theme-colors)
  "Define the faces for a my16 colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (my16-set-faces
   theme-name
   theme-colors
   '(
;;; BUILT-IN
;;;; basic colors
     (border                                        :background zcom)
     (vertical-border                               :background nil :foreground zcom)
     (cursor                                        :background zstr :foreground nil)
     (default                                       :foreground zfg1 :background zbg1)
     (fringe                                        :background nil)
     (gui-element                                   :background zcom)
     (header-line                                   :foreground zkw :box (:color zcom :style pressed-button))
     (highlight                                     :foreground zstr :background zbg2)
     (link                                          :foreground zkw :underline t)
     (link-visited                                  :foreground zcon :underline t)
     (minibuffer-prompt                             :foreground zfg1)
     (region                                        :background zbg3 :foreground zstr)
     (secondary-selection                           :background zcom)
     (trailing-whitespace                           :foreground zbg1 :background zred)
     (widget-button                                 :underline t)
     (widget-field                                  :background zcom)
     (bold                                          :foreground zfg1 :weight bold)
     (italic                                        :foreground zfg1 :slant italic)
     (bold-italic                                   :foreground zfg1 :slant italic :weight bold)
     (ffap                                          :background zcom)
     (header-line-highlight                         :inherit highlight)
     (fixed-pitch                                   :inherit default)
     (fixed-pitch-serif                             :inherit default)
     (variable-pitch                                :inherit default)
     (buffer-menu-buffer                            :foreground zkw :weight normal)
     (bookmark-menu-bookmark                        :foreground zstr :weight normal :height 1.0)
     (bookmark-menu-heading                         :foreground zkw)
     (button                                        :inherit mode-line)
     (line-number                                   :foreground zcom)
     (line-number-current-line                      :foreground zcom :underline t)


     (error                                         :foreground zred :weight normal)
     (warning                                       :foreground zred :weight normal)
     (success                                       :foreground zkw :weight normal)
     (active-hole-face                              :foreground zbg1 :background zkw)
     (alert-high-face                               :foreground zred :weight normal :height 1.0 :inverse-video t)
     (alert-low-face                                :foreground zred)
     (alert-moderate-face                           :foreground zred :weight normal :height 1.0)
     (alert-urgent-face                             :foreground zbg1 :background zred :weight normal :height 1.0)

;;;; compilation
     (compilation-column-number                    :foreground zkw)
     (compilation-line-number                      :foreground zkw)
     (compilation-message-face                     :foreground zkw)
     (compilation-mode-line-exit                   :foreground zkw)
     (compilation-mode-line-fail                   :foreground zkw)
     (compilation-mode-line-run                    :foreground zkw)

;;;; custom
     (custom-variable-tag                          :foreground zfg1)
     (custom-group-tag                             :foreground zfg1)
     (custom-state                                 :foreground zfg1)

;;;; font-lock
     (font-lock-builtin-face                       :foreground zkw)
     (font-lock-comment-delimiter-face             :foreground zcom :slant normal)
     (font-lock-comment-face                       :foreground zcom :slant normal)
     (font-lock-constant-face                      :foreground zcon)
     (font-lock-doc-face                           :foreground zcom)
     (font-lock-doc-string-face                    :foreground zcom)
     (font-lock-function-name-face                 :foreground zkw :weight normal)
     (font-lock-keyword-face                       :foreground zkw)
     (font-lock-negation-char-face                 :foreground zkw)
     (font-lock-preprocessor-face                  :foreground zpre)
     (font-lock-regexp-grouping-backslash          :foreground zstr)
     (font-lock-regexp-grouping-construct          :foreground zstr)
     (font-lock-string-face                        :foreground zgrn)
     (font-lock-type-face                          :foreground zty)
     (font-lock-variable-name-face                 :foreground zfg1)
     (font-lock-warning-face                       :foreground zred)

;;;; isearch
     (match                                        :foreground zkw :background zbg1 :inverse-video t)
     (isearch                                      :foreground zkw :background zbg1 :inverse-video t)
     (isearch-lazy-highlight-face                  :foreground zkw :background zbg1 :inverse-video nil)
     (isearch-fail                                 :background zkw :foreground zbg1)
     (lazy-highlight                               :inherit isearch-lazy-highlight-face)

;;;; mode-line
     (mode-line                                    :foreground zfg3 :box (:color zcom :line-width 2 :style released-button))
     (mode-line-buffer-id                          :foreground zkw :weight normal)
     (mode-line-emphasis                           :foreground zkw)
     (mode-line-highlight                          :foreground zkw :weight normal)
     (mode-line-inactive                           :foreground zcon)
     (mode-line-major-mode-face                    :foreground zkw)
     (mode-line-80col-face                         :foreground zkw)

;;;; doom-modeline     

;;;; haskell
     (haskell-operator-face                         :foreground zfg1)
     (haskell-pragma-face                           :foreground zpre)
     (haskell-definition-face                       :foreground zkw)

;;;; EMMS
     (emms-browser-album-face                       :foreground zkw :weight normal :height 1.3)
     (emms-browser-artist-face                      :foreground zfg1 :weight normal :height 1.3)
     (emms-browser-composer-face                    :inherit emms-browser-artist-face)
     (emms-browser-performer-face                   :inherit emms-browser-artist-face)
     (emms-browser-track-face                       :foreground zfg1)
     (emms-browser-year/genre-face                  :inherit emms-browser-artist-face)
     (emms-metaplaylist-mode-current-face           :foreground zred)
     (emms-metaplaylist-mode-face                   :foreground zfg1)
     (emms-playlist-selected-face                   :inherit highlight)
     (emms-playlist-track-face                      :foreground zfg1)
     (emms-stream-name-face                         :inherit link)
     (emms-stream-url-face                          :inherit link)

;;; THIRD-PARTY

;;;; elfeed
     (elfeed-log-date-face                          :foreground zkw)
     (elfeed-log-debug-level-face                   :inherit error)
     (elfeed-log-error-level-face                   :inherit error)
     (elfeed-log-info-level-face                    :foreground zkw)
     (elfeed-log-warn-level-face                    :foreground zkw)
     (elfeed-search-date-face                       :foreground zcom)
     (elfeed-search-feed-face                       :foreground zkw)
     (elfeed-search-filter-face                     :foreground zkw :weight normal)
     (elfeed-search-last-update-face                :foreground zkw)
     (elfeed-search-tag-face                        :foreground zkw)
     (elfeed-search-title-face                      :foreground zcom)
     (elfeed-search-unread-count-face               :foreground zkw)
     (elfeed-search-unread-title-face               :foreground zfg1 :weight normal :height 1.0)

;;;; elfeed inherited
     (message-cited-text                            :inherit mu4e-cited-1-face)
     (message-header-cc                             :foreground zkw)
     (message-header-name                           :foreground zkw)
     (message-header-newsgroups                     :foreground zkw)
     (message-header-other                          :foreground zkw)
     (message-header-subject                        :foreground zkw)
     (message-header-to                             :foreground zfg1)
     (message-header-xheader                        :inherit link)
     (message-mml                                   :inherit mu4e-header-key-face)
     (message-separator                             :inherit mu4e-compose-separator-face)
	 

;;;; mu4e
     (mu4e-cited-1-face                             :foreground zcom :slant normal)
     (mu4e-cited-2-face                             :foreground zcom :slant normal)
     (mu4e-cited-3-face                             :foreground zcom :slant normal)
     (mu4e-cited-4-face                             :foreground zcom :slant normal)
     (mu4e-cited-5-face                             :inherit mu4e-cited-4-face)
     (mu4e-cited-6-face                             :inherit mu4e-cited-4-face)
     (mu4e-cited-7-face                             :inherit mu4e-cited-4-face)
     (mu4e-replied-face                             :foreground zkw)
     (mu4e-header-face                              :foreground zkw)
     (mu4e-header-marks-face                        :foreground zcom)
     (mu4e-header-key-face                          :foreground zkw)
     (mu4e-header-value-face                        :foreground zfg1)
     (mu4e-header-highlight-face                    :inherit highlight)
     (mu4e-special-header-value-face                :foreground zfg1)
     (mu4e-compose-header-face                      :foreground zfg1 :slant normal)
     (mu4e-compose-separator-face                   :foreground zcom)
     (mu4e-highlight-face                           :inherit highlight)
     (mu4e-unread-face                              :foreground zstr :weight normal :height 1.0)
     (mu4e-flagged-face                             :foreground zred :weight normal :height 1.0)
     (mu4e-contact-face                             :foreground zfg1)
     (mu4e-moved-face                               :foreground zcom :slant normal)
	 (mu4e-context-face								:foreground zstr :weight bold)
	 (mu4e-title-face								:inherit mu4e-context-face)
     (mu4e-system-face                              :foreground zkw)
     (mu4e-warning-face                             :foreground zred)
     (mu4e-region-code                              :inverse-video t)

;;;; auctex
	 (font-latex-bold-face                         :foreground zfg1 :weight bold)
	 (font-latex-doctex-documentation-face         :background zcom)
	 (font-latex-italic-face                       :foreground zkw)
	 (font-latex-math-face                         :foreground zkw)
	 (font-latex-sectioning-0-face                 :foreground zstr :height 1.5 :weight bold)
	 (font-latex-sectioning-1-face                 :foreground zstr :height 1.4 :weight bold)
	 (font-latex-sectioning-2-face                 :foreground zstr :height 1.3 :weight bold)
	 (font-latex-sectioning-3-face                 :foreground zstr :height 1.2 :weight bold)
	 (font-latex-sectioning-4-face                 :foreground zstr :height 1.1 :weight bold)
	 (font-latex-sectioning-5-face                 :foreground zstr :height 1.0 :weight bold)
	 (font-latex-sedate-face                       :inherit font-lock-keyword-face)
	 (font-latex-string-face                       :inherit font-lock-string-face)
	 (font-latex-verbatim-face                     :foreground zfg3)
	 (font-latex-constant-face                     :foreground zfg3)
	 (font-latex-warning-face                      :foreground zred)
	 (font-latex-script-char-face                  :foreground nil :inherit font-latex-math-face)
	 (font-latex-subscript-face                    :inherit font-latex-math-face :height 1)
	 (font-latex-superscript-face                  :inherit font-latex-math-face :height 1)

;;;; company-mode
	 (company-tooltip                              :background zbg1 :foreground zfg1 :box (:line-width 1 :style released-button))
	 (company-scrollbar-bg                         :background zbg1)
	 (company-scrollbar-fg                         :background zcom)
	 (company-tooltip-annotation                   :foreground zkw)
	 (company-tooltip-common                       :foreground zkw)
	 (company-tooltip-selection                    :foreground zbg1 :background zcom)
	 (company-preview                              :foreground zbg1 :background zkw)
	 (company-preview-search                       :foreground zbg1 :background zkw)
	 (company-preview-common                       :inherit secondary-selection)

;;;; csv-mode
	 (csv-separator-face                           :foreground zkw)

;;;; diff-mode
	 (diff-added                                   :foreground zkw)
	 (diff-changed                                 :foreground zkw)
	 (diff-removed                                 :foreground zred)
	 (diff-header                                  :background zcom)
	 (diff-file-header                             :background zcom)
	 (diff-hunk-header                             :foreground zkw :background zcom)

;;;; dired
	 (dired-marked                                 :foreground zstr :weight normal)

;;;; dired+
	 (diredp-compressed-file-suffix                :foreground zkw)
	 (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
	 (diredp-dir-priv                              :foreground zkw :background nil)
	 (diredp-exec-priv                             :foreground zkw :background nil)
	 (diredp-executable-tag                        :foreground zkw :background nil)
	 (diredp-file-name                             :foreground zkw)
	 (diredp-file-suffix                           :foreground zkw)
	 (diredp-flag-mark-line                        :background nil :inherit highlight)
	 (diredp-ignored-file-name                     :foreground zcom)
	 (diredp-link-priv                             :foreground zkw :background nil)
	 (diredp-mode-line-flagged                     :foreground zkw)
	 (diredp-mode-line-marked                      :foreground zkw)
	 (diredp-no-priv                               :background nil)
	 (diredp-number                                :foreground zkw)
	 (diredp-other-priv                            :foreground zkw :background nil)
	 (diredp-rare-priv                             :foreground zkw :background nil)
	 (diredp-read-priv                             :foreground zkw :background nil)
	 (diredp-symlink                               :foreground zkw)
	 (diredp-write-priv                            :foreground zkw :background nil)

;;;; eldoc-mode
	 (eldoc-highlight-function-argument            :foreground zkw :weight bold)

;;;; erc
	 (erc-direct-msg-face                          :foreground zkw)
	 (erc-error-face                               :inherit error)
	 (erc-header-face                              :foreground zfg1 :background zcom)
	 (erc-input-face                               :foreground zkw)
	 (erc-keyword-face                             :foreground zkw)
	 (erc-current-nick-face                        :foreground zkw)
	 (erc-my-nick-face                             :foreground zkw)
	 (erc-nick-default-face                        :foreground zkw :weight normal)
	 (erc-nick-msg-face                            :foreground zkw :weight normal)
	 (erc-notice-face                              :foreground zcom)
	 (erc-pal-face                                 :foreground zkw)
	 (erc-prompt-face                              :foreground zkw)
	 (erc-timestamp-face                           :foreground zkw)

;;;; eshell
	 (eshell-ls-archive                            :foreground zkw)
	 (eshell-ls-backup                             :foreground zkw)
	 (eshell-ls-clutter                            :foreground zkw)
	 (eshell-ls-directory                          :foreground zkw)
	 (eshell-ls-executable                         :foreground zkw)
	 (eshell-ls-missing                            :foreground zkw)
	 (eshell-ls-product                            :foreground zkw)
	 (eshell-ls-readonly                           :foreground zfg1)
	 (eshell-ls-special                            :foreground zkw)
	 (eshell-ls-symlink                            :foreground zkw)
	 (eshell-ls-unreadable                         :foreground zcom)
	 (eshell-prompt                                :foreground zfg1)

;;;; whitespace
	 (whitespace-big-indent						:foreground zbg1 :background zred)
	 (whitespace-empty								:background zred)
	 (whitespace-hspace							:background zred)
	 (whitespace-indentation						:inherit whitespace-big-indent)
	 (whitespace-line								:foreground zred :background zbg1)
	 (whitespace-newline							:inherit whitespace-big-indent)
	 (whitespace-space								:inherit whitespace-empty)
	 (whitespace-space-after-tab					:inherit whitespace-space)
	 (whitespace-space-before-tab					:inherit whitespace-space)
	 (whitespace-tab								:inherit whitespace-space)
	 (whitespace-trailing							:inherit whitespace-big-indent)

;;;; flycheck-mode
	 (flycheck-error                               :underline (:style wave :color zkw))
	 (flycheck-info                                :underline (:style wave :color zkw))
	 (flycheck-warning                             :underline (:style wave :color zkw))

;;;; flymake-mode
	 (flymake-warnline                             :background zcom :underline zkw)
	 (flymake-errline                              :background zcom :underline zkw)

;;;; flyspell-mode
	 (flyspell-duplicate                           :underline (:style wave :color zkw))
	 (flyspell-incorrect                           :underline (:style wave :color zred))

;;;; gnus
	 (gnus-cite-1                                  :foreground nil :inherit outline-1)
	 (gnus-cite-2                                  :foreground nil :inherit outline-2)
	 (gnus-cite-3                                  :foreground nil :inherit outline-3)
	 (gnus-cite-4                                  :foreground nil :inherit outline-4)
	 (gnus-cite-5                                  :foreground nil :inherit outline-5)
	 (gnus-cite-6                                  :foreground nil :inherit outline-6)
	 (gnus-cite-7                                  :foreground nil :inherit outline-7)
	 (gnus-cite-8                                  :foreground nil :inherit outline-8)
	 ;; there are several more -cite- faces...
	 (gnus-header-content                          :inherit message-header-other)
	 (gnus-header-subject                          :inherit message-header-subject)
	 (gnus-header-from                             :foreground zkw :weight normal :inherit message-header-other-face)
	 (gnus-header-name                             :inherit message-header-name)
	 (gnus-button                                  :foreground nil :inherit link)
	 (gnus-signature                               :inherit font-lock-comment-face)

	 (gnus-summary-normal-unread                   :foreground zkw :weight normal)
	 (gnus-summary-normal-read                     :foreground zfg1 :weight normal)
	 (gnus-summary-normal-ancient                  :foreground zkw :weight normal)
	 (gnus-summary-normal-ticked                   :foreground zkw :weight normal)
	 (gnus-summary-low-unread                      :foreground zcom :weight normal)
	 (gnus-summary-low-read                        :foreground zcom :weight normal)
	 (gnus-summary-low-ancient                     :foreground zcom :weight normal)
	 (gnus-summary-high-unread                     :foreground zkw :weight normal)
	 (gnus-summary-high-read                       :foreground zkw :weight normal)
	 (gnus-summary-high-ancient                    :foreground zkw :weight normal)
	 (gnus-summary-high-ticked                     :foreground zkw :weight normal)
	 (gnus-summary-cancelled                       :foreground zkw :background nil :weight normal)

	 (gnus-group-mail-low                          :foreground zcom)
	 (gnus-group-mail-low-empty                    :foreground zcom)
	 (gnus-group-mail-1                            :foreground nil :weight normal :inherit outline-1)
	 (gnus-group-mail-2                            :foreground nil :weight normal :inherit outline-2)
	 (gnus-group-mail-3                            :foreground nil :weight normal :inherit outline-3)
	 (gnus-group-mail-4                            :foreground nil :weight normal :inherit outline-4)
	 (gnus-group-mail-5                            :foreground nil :weight normal :inherit outline-5)
	 (gnus-group-mail-6                            :foreground nil :weight normal :inherit outline-6)
	 (gnus-group-mail-1-empty                      :foreground zcom :inherit gnus-group-mail-1)
	 (gnus-group-mail-2-empty                      :foreground zcom :inherit gnus-group-mail-2)
	 (gnus-group-mail-3-empty                      :foreground zcom :inherit gnus-group-mail-3)
	 (gnus-group-mail-4-empty                      :foreground zcom :inherit gnus-group-mail-4)
	 (gnus-group-mail-5-empty                      :foreground zcom :inherit gnus-group-mail-5)
	 (gnus-group-mail-6-empty                      :foreground zcom :inherit gnus-group-mail-6)
	 (gnus-group-news-1                            :foreground nil :weight normal :inherit outline-5)
	 (gnus-group-news-2                            :foreground nil :weight normal :inherit outline-6)
	 (gnus-group-news-3                            :foreground nil :weight normal :inherit outline-7)
	 (gnus-group-news-4                            :foreground nil :weight normal :inherit outline-8)
	 (gnus-group-news-5                            :foreground nil :weight normal :inherit outline-1)
	 (gnus-group-news-6                            :foreground nil :weight normal :inherit outline-2)
	 (gnus-group-news-1-empty                      :foreground zcom :inherit gnus-group-news-1)
	 (gnus-group-news-2-empty                      :foreground zcom :inherit gnus-group-news-2)
	 (gnus-group-news-3-empty                      :foreground zcom :inherit gnus-group-news-3)
	 (gnus-group-news-4-empty                      :foreground zcom :inherit gnus-group-news-4)
	 (gnus-group-news-5-empty                      :foreground zcom :inherit gnus-group-news-5)
	 (gnus-group-news-6-empty                      :foreground zcom :inherit gnus-group-news-6)

;;;; helm
	 (helm-M-x-key                                 :foreground zkw)
	 (helm-action                                  :foreground zfg1)
	 (helm-buffer-directory                        :foreground zcom :background nil :weight normal)
	 (helm-buffer-file                             :foreground zkw)
	 (helm-buffer-not-saved                        :foreground zkw)
	 (helm-buffer-process                          :foreground zcom)
	 (helm-buffer-saved-out                        :foreground zkw)
	 (helm-buffer-size                             :foreground zkw)
	 (helm-candidate-number                        :foreground zbg1 :background zkw)
	 (helm-ff-directory                            :foreground zcom :background nil :weight normal)
	 (helm-ff-executable                           :foreground zkw)
	 (helm-ff-file                                 :foreground zkw)
	 (helm-ff-invalid-symlink                      :foreground zbg1 :background zkw)
	 (helm-ff-prefix                               :foreground nil :background nil)
	 (helm-ff-symlink                              :foreground zbg1 :background zkw)
	 (helm-grep-cmd-line                           :foreground zkw)
	 (helm-grep-file                               :foreground zkw)
	 (helm-grep-finish                             :foreground zbg1 :background zkw)
	 (helm-grep-lineno                             :foreground zcom)
	 (helm-grep-match                              :foreground zkw)
	 (helm-grep-running                            :foreground zkw)
	 (helm-header                                  :foreground zkw :background zbg1 :underline nil)
	 (helm-match                                   :foreground zkw)
	 (helm-moccur-buffer                           :foreground zkw)
	 (helm-selection                               :foreground nil :background zcom :underline nil)
	 (helm-selection-line                          :foreground nil :background zcom)
	 (helm-separator                               :foreground zcom)
	 (helm-source-header                           :foreground zfg1 :background zcom :weight normal)
	 (helm-visible-mark                            :foreground zbg1 :background zkw)

;;;; hl-line-mode
	 (hl-line                                      :inherit highlight)
	 (col-highlight                                :inherit highlight)

;;;; hl-sexp-mode
	 (hl-sexp-face                                 :background zcom)

;;;; idris-mode
	 (idris-semantic-bound-face                    :inherit font-lock-variable-name-face)
	 (idris-semantic-data-face                     :inherit font-lock-string-face)
	 (idris-semantic-function-face                 :inherit font-lock-function-name-face)
	 (idris-semantic-namespace-face                nil)
	 (idris-semantic-postulate-face                :inherit font-lock-builtin-face)
	 (idris-semantic-type-face                     :inherit font-lock-type-face)
	 (idris-active-term-face                       :inherit highlight)
	 (idris-colon-face                             :inherit font-lock-keyword-face)
	 (idris-equals-face                            :inherit font-lock-keyword-face)
	 (idris-operator-face                          :inherit font-lock-keyword-face)

;;;; ido-mode
	 (ido-first-match                              :foregrond zstr :weight normal)
	 (ido-incomplete-regexp                        :foreground zkw)
	 (ido-indicator                                :foreground zbg1 :background zkw)
	 (ido-only-match                               :foreground zkw)
	 (ido-subdir                                   :foreground zkw)
	 (ido-virtual                                  :foreground zkw)


;;;; ivy-mode
	 (ivy-current-match                            :foreground zkw :background nil :height 1.1 :underline t)
	 (ivy-minibuffer-match-face-1                  :foreground zkw)
	 (ivy-minibuffer-match-face-2                  :foreground zkw)
	 (ivy-minibuffer-match-face-3                  :foreground zkw)
	 (ivy-minibuffer-match-face-4                  :foreground zkw)
	 (ivy-confirm-face                             :foreground zkw)
	 (ivy-match-required-face                      :foreground zkw)
	 (ivy-virtual                                  :foreground zcom)
	 (ivy-action                                   :foreground zkw)
	 (ivy-prompt-match                             :foreground zkw :background nil)
	 (ivy-highlight-face                           :inherit highlight)
	 (ivy-minibuffer-match-highlight               :inherit highlight)
	 (ivy-yanked-word                              :inherit highlight)

;;;; linum-mode
	 (linum                                        :foreground zcom :background zcom)

;;;; markdown-mode
	 (markdown-header-face-1                       :inherit org-level-1)
	 (markdown-header-face-2                       :inherit org-level-2)
	 (markdown-header-face-3                       :inherit org-level-3)
	 (markdown-header-face-4                       :inherit org-level-4)
	 (markdown-url-face                            :inherit link)
	 (markdown-link-face                           :foreground nil :inherit link)
	 (markdown-plain-url-face                      :inherit link)
	 (markdown-pre-face                            :foreground zfg1)
	 (markdown-inline-code-face                    :foreground nil :inherit markdown-pre-face)

;;;; org-mode
	 (org-verbatim									:inherit org-code)
	 (org-todo                                     :foreground zred)
	 (org-block-begin-line                         :foreground zcom)
	 (org-block-end-line                           :inherit org-block-begin-line)
	 (org-agenda-structure                         :foreground zkw)
	 (org-agenda-date                              :foreground zkw :underline nil)
	 (org-agenda-done                              :foreground zkw)
	 (org-agenda-dimmed-todo-face                  :foreground zcom)
	 (org-block                                    :foreground zfg1 :background nil) ;; "#afafaf")
	 (org-code                                     :foreground zkw)
	 (org-column                                   :background zcom)
	 (org-column-title                             :weight bold :underline t :inherit org-column)
	 (org-date                                     :foreground zkw :underline t)
	 (org-document-info                            :foreground zkw)
	 (org-document-info-keyword                    :foreground zcom)
	 (org-document-title                           :foreground zkw :weight bold :height 1.6)
	 (org-done                                     :foreground zkw)
	 (org-meta-line                                :foreground nil :inherit font-lock-comment-face :slant normal)
	 (org-ellipsis                                 :foreground zcom)
	 (org-footnote                                 :foreground zkw)
	 (org-formula                                  :foreground zkw)
	 (org-hide                                     :foreground zcom)
	 (org-link                                     :inherit link)
	 (org-scheduled                                :foreground zkw)
	 (org-scheduled-previously                     :foreground zkw)
	 (org-scheduled-today                          :foreground zkw)
	 (org-special-keyword                          :foreground zkw)
	 (org-table                                    :foreground zkw)
	 (org-todo                                     :foreground zkw)
	 (org-upcoming-deadline                        :foreground zkw)
	 (org-warning                                  :foreground zred :weight normal)
	 (org-level-1                                  :foreground zstr :height 1.3 :weight bold)
	 (org-level-2                                  :foreground zcon :height 1.2 :weight bold)
	 (org-level-3                                  :foreground zkw :height 1.1 :weight bold)
	 (org-level-4                                  :foreground zgrn :height 1.0 :weight bold)
	 (org-level-5                                  :foreground zpre :height 1.0 :weight bold)
	 (org-level-6                                  :foreground zty :height 1.0 :weight bold)
	 (org-level-7                                  :foreground zxb2 :height 1.0 :weight bold)
	 (org-level-8                                  :foreground zty :height 1.0 :weight bold)

;;;; calendar
	 (calendar-today                               :foreground zbg1 :background zkw)
	 (calendar-weekend-header                      :foreground zkw)
	 (calendar-weekday-header                      :foreground zkw)
	 (calendar-month-header                        :foreground zkw :weight bold)

;;;; rainbow-delimiters
	 (rainbow-delimiters-depth-1-face              :foreground zkw)
	 (rainbow-delimiters-depth-2-face              :foreground zty)
	 (rainbow-delimiters-depth-3-face              :foreground zcon)
	 (rainbow-delimiters-depth-4-face              :foreground zstr)
	 (rainbow-delimiters-depth-5-face              :foreground zgrn)
	 (rainbow-delimiters-depth-6-face              :foreground zpre)
	 (rainbow-delimiters-depth-7-face              :foreground zty)
	 (rainbow-delimiters-depth-8-face              :foreground zxb2)
	 (rainbow-delimiters-depth-9-face              :foreground zfg3)
	 (rainbow-delimiters-mismatched-face           :foreground zbg1 :background zred :weight normal :inverse-video t)
	 (rainbow-delimiters-unmatched-face            :inherit rainbow-delimiters-mismatched-face)

;;;; twittering
	 (twittering-username-face                     :foreground zkw :weight normal :inverse-video t)

;;;; sh-mode
	 (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
	 (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
	 (show-paren-match                             :foreground zbg1 :background zgrn)
	 (show-paren-mismatch                          :foreground zbg1 :background zred)
	 (show-paren-match-expression				   :inherit show-paren-match)

;;;; MISC IDK
	 (mm-command-output                            :foreground zkw)
	 (homoglyph                                    :foreground zkw) ;; for unrecognized chars
	 (nobreak-hyphen                               :foreground zred)
	 (nobreak-space                                :foreground zred :underline t)
	 (outline-1                                    :foreground zkw :height 1.3)
	 (outline-2                                    :foreground zkw :height 1.2)
	 (outline-3                                    :foreground zkw :height 1.1)
	 (outline-4                                    :foreground zkw :height 1.0)
	 (outline-5                                    :foreground zkw :height 1.0)
	 (outline-6                                    :foreground zkw :height 1.0)
	 (outline-7                                    :foreground zkw :height 1.0)
	 (outline-8                                    :foreground zkw :height 1.0)
	 (diary                                        :foreground zkw)

;;;; PACKAGES
	 (package-name                                 :foreground zkw :underline nil)
	 (package-status-built-in                      :foreground zgrn)
	 (package-status-installed                     :foreground zstr)
	 (package-status-new                           :foreground zcon :inverse-video t)

;;;; CUSTOMIZE
	 (custom-button                                :foreground zfg1 :box (:color zcom :line-width 2 :style released-button))
	 (custom-button-pressed                        :foreground zfg1 :box (:color zcom :line-width 2 :style pressed-button))
	 (custom-button-pressed-unraised               :foreground zkw :underline t)
	 (custom-button-mouse                          :inherit custom-button)
	 (custom-changed                               :foreground zfg1 :background zkw)
	 (custom-comment-tag                           :foreground zcom)
	 (custom-comment                               :foreground zcom)
	 (custom-documentation                         :foreground zfg1)
	 (custom-face-tag                              :foreground zkw :height 1.1)
	 (custom-group-subtitle                        :foreground zfg1 :weight bold)
	 (custom-group-tag                             :foreground zkw :height 1.1)
	 (custom-group-tag-1                           :foreground zkw :height 1.2 :weight bold)
	 (custom-invalid                               :foreground zred :background nil)
	 (custom-modified                              :foreground zfg1 :background zkw)
	 (custom-rogue                                 :background zbg1 :foreground zkw)
	 (custom-saved                                 :foreground zfg1 :underline t)
	 (custom-set                                   :background zfg1 :foreground zkw)
	 (custom-state                                 :foreground zkw)
	 (custom-themed                                :background zkw :foreground zfg1)
	 (custom-variable-button                       :foreground zfg1 :underline t)
	 (custom-variable-tag                          :foreground zkw :height 1.1)
	 (custom-visibility                            :inherit link :height 1.0)

;;;; INFO
	 (info-title-1                                 :foreground zkw :height 1.3)
	 (info-title-2                                 :foreground zkw :height 1.2)
	 (info-title-3                                 :foreground zkw :height 1.1)
	 (info-title-4                                 :foreground zkw :height 1.1)
	 (info-header-node                             :foreground zfg1 :weight bold)
	 (info-header-xref                             :inherit link)
	 (info-menu-header                             :foreground zfg1 :weight bold)
	 (info-menu-star                               :foreground zkw)
	 (info-node                                    :foreground zfg1)

;;;; EWW
	 (eww-invalid-certificate                      :foreground zred)
	 (eww-valid-certificate                        :foreground zkw)


;;;; AGDA
	 (agda2-highlight-catchall-clause-face         :background zkw :foreground zbg1)
	 (agda2-highlight-coinductive-constructor-face :foreground zkw)
	 (agda2-highlight-bound-variable-face          :foreground zfg1)
	 (agda2-highlight-coverage-problem-face        :foreground zbg1 :background zkw)
	 (agda2-highlight-datatype-face                :foreground zkw)
	 (agda2-highlight-deadcode-face                :foreground zfg1 :background zcom)
	 (agda2-highlight-dotted-face                  :inherit default)
	 (agda2-highlight-error-face                   :inherit error)
	 (agda2-highlight-field-face                   :foreground zkw)
	 (agda2-highlight-function-face                :foreground zkw)
	 (agda2-highlight-generalizable-variable-face  :inherit font-lock-variable-name-face)
	 (agda2-highlight-inductive-constructor-face   :foreground zkw)
	 (agda2-highlight-keyword-face                 :inherit font-lock-keyword-face)
	 (agda2-highlight-macro-face                   :foreground zkw)
	 (agda2-highlight-module-face                  :foreground zkw)
	 (agda2-highlight-number-face                  :foreground zkw)
	 (agda2-highlight-operator-face                :foreground zkw)
	 (agda2-highlight-positivity-problem-face      :background zkw :foreground zbg1)
	 (agda2-highlight-postulate-face               :foreground zkw :weight bold)
	 (agda2-highlight-pragma-face                  :foreground zkw)
	 (agda2-highlight-primitive-face               :inherit font-lock-keyword-face) 
	 (agda2-highlight-primitive-type-face          :inherit font-lock-type-face)
	 (agda2-highlight-record-face                  :inherit font-lock-type-face)
	 (agda2-highlight-string-face                  :inherit font-lock-string-face)
	 (agda2-highlight-symbol-face                  :foreground zkw) 
	 (agda2-highlight-termination-problem-face     :foreground zkw)
	 (agda2-highlight-typechecks-face              :background zkw :foreground zbg1)
	 (agda2-highlight-unsolved-constraint-face     :background zkw :foreground zbg1)
	 (agda2-highlight-unsolved-meta-face           :background zkw :foreground zbg1)

;;;; COQ
	 (coq-button-face                              :foreground zfg1 :box (:color zcom :style released-button))
	 (coq-button-face-pressed                      :foreground zfg1 :box (:color zcom :style pressed-button))
	 (coq-cheat-face                               :foreground zkw)
	 (coq-context-qualifier-face                   :foreground zkw :weight bold)
	 (coq-question-mark-face                       :foreground zkw)
	 (coq-solve-tactics-face                       :foreground zkw)
	 (coq-symbol-binder-face                       :foreground zkw)
	 (coq-symbol-face                              :foreground zkw)

;;;; slime-mode
	 (slime-highlight-edits-face                   :weight bold)
	 (slime-repl-input-face                        :weight normal :underline nil)
	 (slime-repl-prompt-face                       :foreground zkw :underline nil :weight bold)
	 (slime-repl-result-face                       :foreground zkw)
	 (slime-repl-output-face                       :foreground zkw :background zcom)

;;;; term and ansi-term
	 (term                                         :foreground zfg1 :background zbg1)
	 (term-color-black                             :foreground zcom :background zbg1)
	 (term-color-white                             :foreground zfg1 :background zfg1)
	 (term-color-red                               :foreground zred :background nil)
	 (term-color-yellow                            :foreground zkw :background zkw)
	 (term-color-green                             :foreground zkw :background zkw)
	 (term-color-cyan                              :foreground zkw :background zkw)
	 (term-color-blue                              :foreground zkw :background zkw)
	 (term-color-magenta                           :foreground zkw :background zkw)

;;;; undo-tree-mode
	 (undo-tree-visualizer-default-face            :foreground zfg1)
	 (undo-tree-visualizer-current-face            :foreground zkw :weight bold)
	 (undo-tree-visualizer-active-branch-face      :foreground zkw)
	 (undo-tree-visualizer-register-face           :foreground zkw)

;;;; whitespace-mode
	 (whitespace-empty                             :foreground zkw :background zkw)
	 (whitespace-hspace                            :foreground zcom :background zcom)
	 (whitespace-indentation                       :foreground zkw :background zcom)
	 (whitespace-line                              :foreground zkw :background zcom)
	 (whitespace-newline                           :foreground zcom)
	 (whitespace-space                             :foreground zcom :background zcom)
	 (whitespace-space-after-tab                   :foreground zkw :background zcom)
	 (whitespace-space-before-tab                  :foreground zkw :background zcom)
	 (whitespace-tab                               :foreground zcom :background zcom)
	 (whitespace-trailing                          :foreground zkw :background zcom)))


  ;; Anything leftover that doesn't fall neatly into a face goes here.
  (let ((zbg1 (plist-get theme-colors :zbg1))
		(zcom (plist-get theme-colors :zcom))
		(zcom (plist-get theme-colors :zcom))
		(zcom (plist-get theme-colors :zcom))
		(zcom (plist-get theme-colors :zcom))
		(zfg1 (plist-get theme-colors :zfg1))
		(zfg1 (plist-get theme-colors :zfg1))
		(zfg1 (plist-get theme-colors :zfg1))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw))
		(zkw (plist-get theme-colors :zkw)))))

;;;###autoload
;; (and load-file-name
;;      (boundp 'custom-theme-load-path)
;;      (add-to-list 'custom-theme-load-path
;;                   (file-name-as-directory
;;                    (file-name-directory load-file-name))))

(provide 'my16-theme)

;;; my16-theme.el ends here
