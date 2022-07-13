;;; Code:

(defun light-transform-spec (spec colors)
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
          (setq output (append output (list key (light-transform-spec value colors)))))
         (color
          (setq output (append output (list key color))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun light-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face         (car spec))
         (definition   (cdr spec)))
    (list face `((((type graphic)) ,(light-transform-spec definition colors))))))


(defun light-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (light-transform-face face colors))
                 faces)))

(defun light-theme-define (theme-name theme-colors)
  "Define the faces for a light colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (light-set-faces
   theme-name
   theme-colors
   '(
;;; BUILT-IN
;;;; basic colors
     (border                                       :background zgrey1)
     (vertical-border                               :background nil :foreground zgrey2)
     (cursor                                       :background zmagenta :foreground nil)
     (default                                      :foreground zblack :background zwhite)
     (fringe                                       :background nil)
     (gui-element                                  :background zgrey3)
     (header-line                                  :foreground zred :background nil :inherit mode-line)
     (highlight                                    :background zgrey1)
     (link                                         :foreground zblue :underline t)
     (link-visited                                 :foreground zred :underline t)
     (minibuffer-prompt                            :foreground zblue)
     (region                                       :background zyellow :foreground zwhite)
     (secondary-selection                          :background zgrey2)
     (trailing-whitespace                          :foreground zyellow :background zgrey1)
     (widget-button                                :underline t)
     (widget-field                                 :background zgrey2 :box (:line-width 1 :color zblack))
     (bold                                          :foreground zblack :weight bold)
     (italic                                        :foreground zblack :slant normal)
     (bold-italic                                   :foreground zblack :slant italic :weight bold)
     (ffap                                          :background zgrey1)
     (header-line-highlight                         :background zgrey1)
     (fixed-pitch                                   :inherit default)
     (fixed-pitch-serif                             :inherit default)
     (variable-pitch                                :inherit default)
     (buffer-menu-buffer                            :foreground zyellow :weight bold)
     (bookmark-menu-bookmark                        :foreground zyellow :weight bold :height 1.1)
     (bookmark-menu-heading                         :foreground zcyan)


     (error                                        :foreground zred :weight bold)
     (warning                                      :foreground zorange :weight bold)
     (success                                      :foreground zgreen :weight bold)

;;;; compilation
     (compilation-column-number                    :foreground zyellow)
     (compilation-line-number                      :foreground zyellow)
     (compilation-message-face                     :foreground zblue)
     (compilation-mode-line-exit                   :foreground zgreen)
     (compilation-mode-line-fail                   :foreground zred)
     (compilation-mode-line-run                    :foreground zblue)

;;;; custom
     (custom-variable-tag                          :foreground zblue)
     (custom-group-tag                             :foreground zblue)
     (custom-state                                 :foreground zgreen)

;;;; font-lock
     (font-lock-builtin-face                       :foreground zred)
     (font-lock-comment-delimiter-face             :foreground zgrey3 :slant normal)
     (font-lock-comment-face                       :foreground zgrey4 :slant normal)
     (font-lock-constant-face                      :foreground zorange)
     (font-lock-doc-face                           :foreground zgrey4)
     (font-lock-doc-string-face                    :foreground zgrey2)
     (font-lock-function-name-face                 :foreground zcyan)
     (font-lock-keyword-face                       :foreground zblue)
     (font-lock-negation-char-face                 :foreground zgreen)
     (font-lock-preprocessor-face                  :foreground zblue)
     (font-lock-regexp-grouping-backslash          :foreground zyellow)
     (font-lock-regexp-grouping-construct          :foreground zred)
     (font-lock-string-face                        :foreground zgreen)
     (font-lock-type-face                          :foreground zcyan)
     (font-lock-variable-name-face                 :foreground zred)
     (font-lock-warning-face                       :foreground zred)

;;;; isearch
     (match                                        :foreground zblue :background pureblack :inverse-video t)
     (isearch                                      :foreground zyellow :background pureblack :inverse-video t)
     (isearch-lazy-highlight-face                  :foreground zcyan :background pureblack :inverse-video nil)
     (isearch-fail                                 :background zred :foreground zwhite)
     (lazy-highlight                               :inherit isearch-lazy-highlight-face)

;;;; mode-line
     (mode-line                                    :background zgrey2 :box (:color zgrey2))
     (mode-line-buffer-id                          :foreground zwhite :weight bold)
     (mode-line-emphasis                           :foreground zyellow)
     (mode-line-highlight                          :foreground zred :box nil :weight bold)
     (mode-line-inactive                           :background zyellow :foreground zwhite :box (:color zgrey2))
     (mode-line-major-mode-face                    :foreground zblue)
     (mode-line-80col-face                         :foreground zred)

;;;; haskell
     (haskell-operator-face                         :foreground zyellow)
     (haskell-pragma-face                           :foreground zgrey3)
     (haskell-definition-face                       :foreground zorange)

;;; THIRD-PARTY

;;;; elfeed
     (elfeed-log-date-face                          :foreground zgreen)
     (elfeed-log-debug-level-face                   :foreground zmagenta)
     (elfeed-log-error-level-face                   :foreground zred)
     (elfeed-log-info-level-face                    :foreground zblue)
     (elfeed-log-warn-level-face                    :foreground zyellow)
     (elfeed-search-date-face                       :foreground zgrey2)
     (elfeed-search-feed-face                       :foreground zgreen)
     (elfeed-search-filter-face                     :foreground zblue :weight bold)
     (elfeed-search-last-update-face                :foreground zblue)
     (elfeed-search-tag-face                        :foreground zcyan)
     (elfeed-search-title-face                      :foreground zblack)
     (elfeed-search-unread-count-face               :foreground zblue)
     (elfeed-search-unread-title-face               :foreground zyellow :weight bold :height 1.1)

;;;; elfeed inherited
     (message-cited-text                            :inherit mu4e-cited-1-face)
     (message-header-cc                             :foreground zblue)
     (message-header-name                           :foreground zcyan)
     (message-header-newsgroups                     :foreground zblue)
     (message-header-other                          :foreground zblue)
     (message-header-subject                        :foreground zyellow)
     (message-header-to                             :foreground zblue)
     (message-header-xheader                        :inherit link)
     (message-mml                                   :inherit mu4e-header-key-face)
     (message-separator                             :inherit mu4e-compose-separator-face)
                                                    

;;;; mu4e
     (mu4e-cited-1-face                             :foreground zyellow :slant normal)
     (mu4e-cited-2-face                             :foreground zgreen :slant normal)
     (mu4e-cited-3-face                             :foreground zcyan :slant normal)
     (mu4e-cited-4-face                             :foreground zblue :slant normal)
     (mu4e-replied-face                             :foreground zyellow)
     (mu4e-header-face                              :foreground zblack)
     (mu4e-header-key-face                          :foreground zgreen)
     (mu4e-special-header-value-face                :foreground zyellow)
     (mu4e-compose-header-face                      :foreground zcyan :slant normal)
     (mu4e-compose-separator-face                   :foreground zred :slant normal)
     (mu4e-highlight-face                           :inherit highlight)
     (mu4e-unread-face                              :foreground zcyan :weight bold :height 1.1)
     (mu4e-flagged-face                             :foreground zred :weight bold :height 1.1)
     (mu4e-header-highlight-face                    :inherit highlight :weight bold)
     (mu4e-contact-face                             :foreground zblue)

;;;; auctex
      (font-latex-bold-face                         :foreground zblack :weight bold)
      (font-latex-doctex-documentation-face         :background zgrey2)
      (font-latex-italic-face                       :foreground zcyan)
      (font-latex-math-face                         :foreground zgreen)
      (font-latex-sectioning-0-face                 :foreground zyellow :height 1.5 :weight bold)
      (font-latex-sectioning-1-face                 :foreground zyellow :height 1.4 :weight bold)
      (font-latex-sectioning-2-face                 :foreground zyellow :height 1.3 :weight bold)
      (font-latex-sectioning-3-face                 :foreground zyellow :height 1.2 :weight bold)
      (font-latex-sectioning-4-face                 :foreground zyellow :height 1.1 :weight bold)
      (font-latex-sectioning-5-face                 :foreground zyellow :height 1.0 :weight bold)
      (font-latex-sedate-face                       :inherit font-lock-keyword-face)
      (font-latex-string-face                       :foreground zyellow)
      (font-latex-verbatim-face                     :foreground zorange)
      (font-latex-warning-face                      :foreground zred)
      (font-latex-script-char-face                  :foreground nil :inherit font-latex-math-face)
      (font-latex-subscript-face                    :inherit font-latex-math-face :height 1)
      (font-latex-superscript-face                  :inherit font-latex-math-face :height 1)

;;;; company-mode
      (company-tooltip                              :background zgrey3 :inherit default)
      (company-scrollbar-bg                         :background zblack)
      (company-scrollbar-fg                         :background zgrey4)
      (company-tooltip-annotation                   :foreground zred)
      (company-tooltip-common                       :inherit font-lock-constant-face)
      (company-tooltip-selection                    :background zgrey1 :inherit font-lock-function-name-face)
      (company-preview-common                       :inherit secondary-selection)

;;;; csv-mode
      (csv-separator-face                           :foreground zorange)

;;;; diff-mode
      (diff-added                                   :foreground zgreen)
      (diff-changed                                 :foreground zred)
      (diff-removed                                 :foreground zred)
      (diff-header                                  :background zgrey3)
      (diff-file-header                             :background zgrey1)
      (diff-hunk-header                             :foreground zred :background zgrey3)

;;;; dired
      (dired-marked                                 :foreground zyellow :weight bold)

;;;; dired+
      (diredp-compressed-file-suffix                :foreground zblue)
      (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
      (diredp-dir-priv                              :foreground zcyan :background nil)
      (diredp-exec-priv                             :foreground zblue :background nil)
      (diredp-executable-tag                        :foreground zred :background nil)
      (diredp-file-name                             :foreground zyellow)
      (diredp-file-suffix                           :foreground zgreen)
      (diredp-flag-mark-line                        :background nil :inherit highlight)
      (diredp-ignored-file-name                     :foreground zgrey4)
      (diredp-link-priv                             :foreground zred :background nil)
      (diredp-mode-line-flagged                     :foreground zred)
      (diredp-mode-line-marked                      :foreground zgreen)
      (diredp-no-priv                               :background nil)
      (diredp-number                                :foreground zyellow)
      (diredp-other-priv                            :foreground zred :background nil)
      (diredp-rare-priv                             :foreground zred :background nil)
      (diredp-read-priv                             :foreground zgreen :background nil)
      (diredp-symlink                               :foreground zred)
      (diredp-write-priv                            :foreground zyellow :background nil)

;;;; eldoc-mode
      (eldoc-highlight-function-argument            :foreground zyellow :weight bold)

;;;; erc
      (erc-direct-msg-face                          :foreground zorange)
      (erc-error-face                               :foreground zred)
      (erc-header-face                              :foreground zblack :background zgrey4)
      (erc-input-face                               :foreground zgreen)
      (erc-keyword-face                             :foreground zyellow)
      (erc-current-nick-face                        :foreground zgreen)
      (erc-my-nick-face                             :foreground zgreen)
      (erc-nick-default-face                        :foreground zred :weight normal)
      (erc-nick-msg-face                            :foreground zyellow :weight normal)
      (erc-notice-face                              :foreground zgrey4)
      (erc-pal-face                                 :foreground zorange)
      (erc-prompt-face                              :foreground zblue)
      (erc-timestamp-face                           :foreground zcyan)

;;;; eshell
      (eshell-ls-archive                            :foreground zred)
      (eshell-ls-backup                             :foreground zred)
      (eshell-ls-clutter                            :foreground zorange)
      (eshell-ls-directory                          :foreground zblue)
      (eshell-ls-executable                         :foreground zgreen)
      (eshell-ls-missing                            :foreground zred)
      (eshell-ls-product                            :foreground zred)
      (eshell-ls-readonly                           :foreground zblack)
      (eshell-ls-special                            :foreground zred)
      (eshell-ls-symlink                            :foreground zcyan)
      (eshell-ls-unreadable                         :foreground zgrey4)
      (eshell-prompt                                :foreground zblack)

;;;; flycheck-mode
      (flycheck-error                               :underline (:style wave :color zred))
      (flycheck-info                                :underline (:style wave :color zgreen))
      (flycheck-warning                             :underline (:style wave :color zorange))

;;;; flymake-mode
      (flymake-warnline                             :background zgrey3 :underline zorange)
      (flymake-errline                              :background zgrey3 :underline zred)

;;;; flyspell-mode
      (flyspell-duplicate                           :underline (:style wave :color zorange))
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
      (gnus-header-from                             :foreground zorange :weight bold :inherit message-header-other-face)
      (gnus-header-name                             :inherit message-header-name)
      (gnus-button                                  :foreground nil :inherit link)
      (gnus-signature                               :inherit font-lock-comment-face)

      (gnus-summary-normal-unread                   :foreground zblue :weight normal)
      (gnus-summary-normal-read                     :foreground zblack :weight normal)
      (gnus-summary-normal-ancient                  :foreground zcyan :weight normal)
      (gnus-summary-normal-ticked                   :foreground zorange :weight normal)
      (gnus-summary-low-unread                      :foreground zgrey4 :weight normal)
      (gnus-summary-low-read                        :foreground zgrey4 :weight normal)
      (gnus-summary-low-ancient                     :foreground zgrey4 :weight normal)
      (gnus-summary-high-unread                     :foreground zyellow :weight normal)
      (gnus-summary-high-read                       :foreground zgreen :weight normal)
      (gnus-summary-high-ancient                    :foreground zgreen :weight normal)
      (gnus-summary-high-ticked                     :foreground zorange :weight normal)
      (gnus-summary-cancelled                       :foreground zred :background nil :weight normal)

      (gnus-group-mail-low                          :foreground zgrey4)
      (gnus-group-mail-low-empty                    :foreground zgrey4)
      (gnus-group-mail-1                            :foreground nil :weight normal :inherit outline-1)
      (gnus-group-mail-2                            :foreground nil :weight normal :inherit outline-2)
      (gnus-group-mail-3                            :foreground nil :weight normal :inherit outline-3)
      (gnus-group-mail-4                            :foreground nil :weight normal :inherit outline-4)
      (gnus-group-mail-5                            :foreground nil :weight normal :inherit outline-5)
      (gnus-group-mail-6                            :foreground nil :weight normal :inherit outline-6)
      (gnus-group-mail-1-empty                      :foreground zgrey4 :inherit gnus-group-mail-1)
      (gnus-group-mail-2-empty                      :foreground zgrey4 :inherit gnus-group-mail-2)
      (gnus-group-mail-3-empty                      :foreground zgrey4 :inherit gnus-group-mail-3)
      (gnus-group-mail-4-empty                      :foreground zgrey4 :inherit gnus-group-mail-4)
      (gnus-group-mail-5-empty                      :foreground zgrey4 :inherit gnus-group-mail-5)
      (gnus-group-mail-6-empty                      :foreground zgrey4 :inherit gnus-group-mail-6)
      (gnus-group-news-1                            :foreground nil :weight normal :inherit outline-5)
      (gnus-group-news-2                            :foreground nil :weight normal :inherit outline-6)
      (gnus-group-news-3                            :foreground nil :weight normal :inherit outline-7)
      (gnus-group-news-4                            :foreground nil :weight normal :inherit outline-8)
      (gnus-group-news-5                            :foreground nil :weight normal :inherit outline-1)
      (gnus-group-news-6                            :foreground nil :weight normal :inherit outline-2)
      (gnus-group-news-1-empty                      :foreground zgrey4 :inherit gnus-group-news-1)
      (gnus-group-news-2-empty                      :foreground zgrey4 :inherit gnus-group-news-2)
      (gnus-group-news-3-empty                      :foreground zgrey4 :inherit gnus-group-news-3)
      (gnus-group-news-4-empty                      :foreground zgrey4 :inherit gnus-group-news-4)
      (gnus-group-news-5-empty                      :foreground zgrey4 :inherit gnus-group-news-5)
      (gnus-group-news-6-empty                      :foreground zgrey4 :inherit gnus-group-news-6)

;;;; helm
      (helm-M-x-key                                 :foreground zcyan)
      (helm-action                                  :foreground zblack)
      (helm-buffer-directory                        :foreground zgrey4 :background nil :weight bold)
      (helm-buffer-file                             :foreground zcyan)
      (helm-buffer-not-saved                        :foreground zred)
      (helm-buffer-process                          :foreground zgrey2)
      (helm-buffer-saved-out                        :foreground zred)
      (helm-buffer-size                             :foreground zorange)
      (helm-candidate-number                        :foreground zwhite :background zorange)
      (helm-ff-directory                            :foreground zgrey4 :background nil :weight bold)
      (helm-ff-executable                           :foreground zgreen)
      (helm-ff-file                                 :foreground zcyan)
      (helm-ff-invalid-symlink                      :foreground zwhite :background zred)
      (helm-ff-prefix                               :foreground nil :background nil)
      (helm-ff-symlink                              :foreground zwhite :background zcyan)
      (helm-grep-cmd-line                           :foreground zgreen)
      (helm-grep-file                               :foreground zcyan)
      (helm-grep-finish                             :foreground zwhite :background zorange)
      (helm-grep-lineno                             :foreground zgrey2)
      (helm-grep-match                              :foreground zyellow)
      (helm-grep-running                            :foreground zorange)
      (helm-header                                  :foreground zyellow :background zwhite :underline nil)
      (helm-match                                   :foreground zyellow)
      (helm-moccur-buffer                           :foreground zcyan)
      (helm-selection                               :foreground nil :background zgrey1 :underline nil)
      (helm-selection-line                          :foreground nil :background zgrey1)
      (helm-separator                               :foreground zgrey1)
      (helm-source-header                           :foreground zblack :background zgrey3 :weight bold)
      (helm-visible-mark                            :foreground zwhite :background zgreen)

;;;; hl-line-mode
      (hl-line                                      :background zgrey1)
      (col-highlight                                :background zgrey1)

;;;; hl-sexp-mode
      (hl-sexp-face                                 :background zgrey2)

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

;;;; ivy-mode
      (ivy-current-match                            :foreground zgreen :background nil :height 1.1 :underline t)
      (ivy-minibuffer-match-face-1                  :foreground zred)
      (ivy-minibuffer-match-face-2                  :foreground zblue)
      (ivy-minibuffer-match-face-3                  :foreground zcyan)
      (ivy-minibuffer-match-face-4                  :foreground zgreen)
      (ivy-confirm-face                             :foreground zgreen)
      (ivy-match-required-face                      :foreground zred)
      (ivy-virtual                                  :foreground zgrey4)
      (ivy-action                                   :foreground zblue)
      (ivy-prompt-match                             :foreground zorange :background nil)
      (ivy-highlight-face                           :inherit highlight)
      (ivy-minibuffer-match-highlight               :inherit highlight)
      (ivy-yanked-word                              :inherit highlight)

;;;; linum-mode
      (linum                                        :foreground zgrey2 :background zgrey3)

;;;; markdown-mode
      (markdown-header-face-1                       :inherit org-level-1)
      (markdown-header-face-2                       :inherit org-level-2)
      (markdown-header-face-3                       :inherit org-level-3)
      (markdown-header-face-4                       :inherit org-level-4)
      (markdown-url-face                            :inherit link)
      (markdown-link-face                           :foreground nil :inherit link)
      (markdown-plain-url-face                      :inherit link)
      (markdown-pre-face                            :foreground zred)
      (markdown-inline-code-face                    :foreground nil :inherit markdown-pre-face)

;;;; org-mode
      (org-block-begin-line                         :box (:line-width 1 :color zgrey1) :foreground zgrey3)
      (org-block-end-line                           :inherit org-block-begin-line)
      (org-agenda-structure                         :foreground zred)
      (org-agenda-date                              :foreground zblue :underline nil)
      (org-agenda-done                              :foreground zgreen)
      (org-agenda-dimmed-todo-face                  :foreground zgrey4)
      (org-block                                    :foreground zblack) ;; "#afafaf")
      (org-code                                     :foreground zred)
      (org-column                                   :background zgrey1)
      (org-column-title                             :weight bold :underline t :inherit org-column)
      (org-date                                     :foreground zred :underline t)
      (org-document-info                            :foreground zcyan)
      (org-document-info-keyword                    :foreground zgreen)
      (org-document-title                           :foreground zorange :weight bold :height 1.44)
      (org-done                                     :foreground zgreen)
      (org-meta-line                                :foreground nil :inherit font-lock-comment-face :slant normal)
      (org-ellipsis                                 :foreground zgrey4)
      (org-footnote                                 :foreground zcyan)
      (org-formula                                  :foreground zred)
      (org-hide                                     :foreground zgrey2)
      (org-link                                     :foreground zblue)
      (org-scheduled                                :foreground zgreen)
      (org-scheduled-previously                     :foreground zorange)
      (org-scheduled-today                          :foreground zgreen)
      (org-special-keyword                          :foreground zorange)
      (org-table                                    :foreground zred)
      (org-todo                                     :foreground zred)
      (org-upcoming-deadline                        :foreground zorange)
      (org-warning                                  :foreground zred :weight bold)
      (org-level-1                                  :foreground zyellow :height 1.3 :weight bold)
      (org-level-2                                  :foreground zgreen :height 1.2 :weight bold)
      (org-level-3                                  :foreground zcyan :height 1.1 :weight bold)
      (org-level-4                                  :foreground zblue :height 1.0 :weight bold)
      (org-level-5                                  :foreground zorange :height 1.0 :weight bold)
      (org-level-6                                  :foreground zorange :height 1.0 :weight bold)
      (org-level-7                                  :foreground zorange :height 1.0 :weight bold)
      (org-level-8                                  :foreground zorange :height 1.0 :weight bold)

;;;; rainbow-delimiters
      (rainbow-delimiters-depth-1-face              :foreground zred)
      (rainbow-delimiters-depth-2-face              :foreground zblue)
      (rainbow-delimiters-depth-3-face              :foreground zcyan)
      (rainbow-delimiters-depth-4-face              :foreground zgreen)
      (rainbow-delimiters-depth-5-face              :foreground zyellow)
      (rainbow-delimiters-depth-6-face              :foreground zorange)
      (rainbow-delimiters-depth-7-face              :foreground zred)
      (rainbow-delimiters-depth-8-face              :foreground zgrey2)
      (rainbow-delimiters-depth-9-face              :foreground zblack)
      (rainbow-delimiters-mismatched-face           :foreground pureblack :background zyellow :weight bold)
      (rainbow-delimiters-unmatched-face            :inherit rainbow-delimiters-mismatched-face)

;;;; sh-mode
      (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
      (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
      (show-paren-match                             :foreground pureblack :background zblue)
      (show-paren-mismatch                          :foreground pureblack :background zorange)

;;;; MISC IDK
      (mm-command-output                            :foreground zgreen)
      (homoglyph                                    :foreground zcyan) ;; for unrecognized chars
      (nobreak-hyphen                               :foreground zcyan)
      (nobreak-space                                :foreground zcyan :underline t)
      (outline-1                                    :foreground zyellow :height 1.3)
      (outline-2                                    :foreground zgreen :height 1.2)
      (outline-3                                    :foreground zcyan :height 1.1)
      (outline-4                                    :foreground zblue :height 1.0)
      (outline-5                                    :foreground zorange :height 1.0)
      (outline-6                                    :foreground zorange :height 1.0)
      (outline-7                                    :foreground zorange :height 1.0)
      (outline-8                                    :foreground zorange :height 1.0)
      (diary                                        :foreground zyellow)

;;;; PACKAGES
      (package-name                                 :foreground zcyan :underline t)
      (package-status-built-in                      :foreground zblue)
      (package-status-installed                     :foreground zgreen)
      (package-status-new                           :foreground zyellow :weight bold)

;;;; CUSTOMIZE
      (custom-changed                               :foreground zblack :background zblue)
      (custom-button-pressed-unraised               :foreground zblue :underline t)
      (custom-comment-tag                           :foreground zgrey2)
      (custom-comment                               :foreground zgrey3)
      (custom-documentation                         :foreground zblack)
      (custom-face-tag                              :foreground zblue :height 1.1)
      (custom-group-subtitle                        :foreground zblack :weight bold)
      (custom-group-tag                             :foreground zblue :height 1.1)
      (custom-group-tag-1                           :foreground zorange :height 1.2 :weight bold)
      (custom-invalid                               :foreground zwhite :background zred)
      (custom-modified                              :foreground zblack :background zblue)
      (custom-rogue                                 :background zwhite :foreground zred)
      (custom-saved                                 :foreground zblack :underline t)
      (custom-set                                   :background zblack :foreground zblue)
      (custom-state                                 :foreground zgreen)
      (custom-themed                                :background zblue :foreground zblack)
      (custom-variable-button                       :foreground zblack :underline t)
      (custom-variable-tag                          :foreground zblue :height 1.1)
      (custom-visibility                            :inherit link :height 1.0)

;;;; INFO
      (info-title-1                                 :foreground zyellow :height 1.3)
      (info-title-2                                 :foreground zgreen :height 1.2)
      (info-title-3                                 :foreground zcyan :height 1.1)
      (info-title-4                                 :foreground zblue :height 1.1)
    

;;;; EWW
      (eww-invalid-certificate                      :foreground zred)
      (eww-valid-certificate                        :foreground zgreen)
      

;;;; slime-mode
      (slime-highlight-edits-face                   :weight bold)
      (slime-repl-input-face                        :weight normal :underline nil)
      (slime-repl-prompt-face                       :foreground zred :underline nil :weight bold)
      (slime-repl-result-face                       :foreground zgreen)
      (slime-repl-output-face                       :foreground zblue :background zgrey3)

;;;; term and ansi-term
      (term                                         :foreground zblack :background zwhite)
      (term-color-black                             :foreground zgrey1 :background zwhite)
      (term-color-white                             :foreground zblack :background zblack)
      (term-color-red                               :foreground zred :background zred)
      (term-color-yellow                            :foreground zyellow :background zyellow)
      (term-color-green                             :foreground zgreen :background zgreen)
      (term-color-cyan                              :foreground zcyan :background zcyan)
      (term-color-blue                              :foreground zblue :background zblue)
      (term-color-magenta                           :foreground zred :background zred)

;;;; undo-tree-mode
      (undo-tree-visualizer-default-face            :foreground zblack)
      (undo-tree-visualizer-current-face            :foreground zgreen :weight bold)
      (undo-tree-visualizer-active-branch-face      :foreground zred)
      (undo-tree-visualizer-register-face           :foreground zyellow)

;;;; whitespace-mode
      (whitespace-empty                             :foreground zred :background zyellow)
      (whitespace-hspace                            :foreground zgrey4 :background zgrey1)
      (whitespace-indentation                       :foreground zred :background zgrey1)
      (whitespace-line                              :foreground zred :background zgrey1)
      (whitespace-newline                           :foreground zgrey4)
      (whitespace-space                             :foreground zgrey2 :background zgrey1)
      (whitespace-space-after-tab                   :foreground zred :background zgrey1)
      (whitespace-space-before-tab                  :foreground zred :background zgrey1)
      (whitespace-tab                               :foreground zgrey2 :background zgrey1)
      (whitespace-trailing                          :foreground zyellow :background zgrey1)))

   ;; Anything leftover that doesn't fall neatly into a face goes here.
   (let ((zwhite (plist-get theme-colors :zwhite))
         (zgrey3 (plist-get theme-colors :zgrey3))
         (zgrey1 (plist-get theme-colors :zgrey1))
         (zgrey2 (plist-get theme-colors :zgrey2))
         (zgrey4 (plist-get theme-colors :zgrey4))
         (zblack (plist-get theme-colors :zblack))
         (zblack (plist-get theme-colors :zblack))
         (zblack (plist-get theme-colors :zblack))
         (zred (plist-get theme-colors :zred))
         (zorange (plist-get theme-colors :zorange))
         (zyellow (plist-get theme-colors :zyellow))
         (zgreen (plist-get theme-colors :zgreen))
         (zcyan (plist-get theme-colors :zcyan))
         (zblue (plist-get theme-colors :zblue))
         (zred (plist-get theme-colors :zred))
         (zred (plist-get theme-colors :zred)))))

;;;###autoload
;; (and load-file-name
;;      (boundp 'custom-theme-load-path)
;;      (add-to-list 'custom-theme-load-path
;;                   (file-name-as-directory
;;                    (file-name-directory load-file-name))))

(provide 'light-theme)

;;; light-theme.el ends here
