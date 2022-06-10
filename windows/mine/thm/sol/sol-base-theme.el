;;; Code:

(defun sol-transform-spec (spec colors)
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
          (setq output (append output (list key (sol-transform-spec value colors)))))
         (color
          (setq output (append output (list key color))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun sol-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face         (car spec))
         (definition   (cdr spec)))
    (list face `((((type graphic)) ,(sol-transform-spec definition colors))))))


(defun sol-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (sol-transform-face face colors))
                 faces)))

(defun sol-theme-define (theme-name theme-colors)
  "Define the faces for a sol colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (sol-set-faces
   theme-name
   theme-colors
   '(
;;; BUILT-IN
;;;; basic colors
     (border                                       :background zgrey1)
     (vertical-border                               :background nil :foreground zgrey2)
     (cursor                                       :background zcyan :foreground nil)
     (default                                      :foreground zwhite1 :background zblack)
     (fringe                                       :background nil)
     (gui-element                                  :background zgrey3)
     (header-line                                  :foreground zgreen :background nil :inherit mode-line)
     (highlight                                    :background zgrey1)
     (link                                         :foreground zblue :underline t)
     (link-visited                                 :foreground zgreen :underline t)
     (minibuffer-prompt                            :foreground zblue)
     (region                                       :background zblue :foreground zblack)
     (secondary-selection                          :background zgrey2)
     (trailing-whitespace                          :foreground zblue :background zgrey1)
     (widget-button                                :underline t)
     (widget-field                                 :background zgrey2 :box (:line-width 1 :color zwhite1))
     (bold                                          :foreground zwhite1 :weight bold)
     (italic                                        :foreground zwhite1 :slant normal)
     (bold-italic                                   :foreground zwhite1 :slant italic :weight bold)
     (ffap                                          :background zgrey1)
     (header-line-highlight                         :background zgrey1)
     (fixed-pitch                                   :inherit default)
     (fixed-pitch-serif                             :inherit default)
     (variable-pitch                                :inherit default)
     (buffer-menu-buffer                            :foreground zblue :weight bold)
     (bookmark-menu-bookmark                        :foreground zblue :weight bold :height 1.1)
     (bookmark-menu-heading                         :foreground zcyan)


     (error                                        :foreground zgreen :weight bold)
     (warning                                      :foreground zgreen :weight bold)
     (success                                      :foreground zcyan :weight bold)

;;;; compilation
     (compilation-column-number                    :foreground zblue)
     (compilation-line-number                      :foreground zblue)
     (compilation-message-face                     :foreground zblue)
     (compilation-mode-line-exit                   :foreground zcyan)
     (compilation-mode-line-fail                   :foreground zgreen)
     (compilation-mode-line-run                    :foreground zblue)

;;;; custom
     (custom-variable-tag                          :foreground zblue)
     (custom-group-tag                             :foreground zblue)
     (custom-state                                 :foreground zcyan)

;;;; font-lock
     (font-lock-builtin-face                       :foreground zgreen)
     (font-lock-comment-delimiter-face             :foreground zgrey1 :slant normal)
     (font-lock-comment-face                       :foreground zgrey2 :slant normal)
     (font-lock-constant-face                      :foreground zgreen)
     (font-lock-doc-face                           :foreground zgrey4)
     (font-lock-doc-string-face                    :foreground zgrey2)
     (font-lock-function-name-face                 :foreground zcyan)
     (font-lock-keyword-face                       :foreground zblue)
     (font-lock-negation-char-face                 :foreground zcyan)
     (font-lock-preprocessor-face                  :foreground zblue)
     (font-lock-regexp-grouping-backslash          :foreground zblue)
     (font-lock-regexp-grouping-construct          :foreground zgreen)
     (font-lock-string-face                        :foreground zcyan)
     (font-lock-type-face                          :foreground zcyan)
     (font-lock-variable-name-face                 :foreground zgreen)
     (font-lock-warning-face                       :foreground zgreen)

;;;; isearch
     (match                                        :foreground zblue :background pureblack :inverse-video t)
     (isearch                                      :foreground zcyan :background pureblack :inverse-video t)
     (isearch-lazy-highlight-face                  :foreground zcyan :background pureblack :inverse-video nil)
     (isearch-fail                                 :background zgreen :foreground zblack)
     (lazy-highlight                               :inherit isearch-lazy-highlight-face)

;;;; mode-line
     (mode-line                                    :foreground zwhite1 :box (:color zgrey2))
     (mode-line-buffer-id                          :foreground zcyan :weight bold)
     (mode-line-emphasis                           :foreground zblue)
     (mode-line-highlight                          :foreground zgreen :box nil :weight bold)
     (mode-line-inactive                           :foreground zgrey2)
     (mode-line-major-mode-face                    :foreground zblue)
     (mode-line-80col-face                         :foreground zgreen)

;;;; haskell
     (haskell-operator-face                         :foreground zblue)
     (haskell-pragma-face                           :foreground zgrey3)
     (haskell-definition-face                       :foreground zgreen)

;;; THIRD-PARTY

;;;; elfeed
     (elfeed-log-date-face                          :foreground zcyan)
     (elfeed-log-debug-level-face                   :foreground zmagenta)
     (elfeed-log-error-level-face                   :foreground zgreen)
     (elfeed-log-info-level-face                    :foreground zblue)
     (elfeed-log-warn-level-face                    :foreground zblue)
     (elfeed-search-date-face                       :foreground zgrey2)
     (elfeed-search-feed-face                       :foreground zcyan)
     (elfeed-search-filter-face                     :foreground zblue :weight bold)
     (elfeed-search-last-update-face                :foreground zblue)
     (elfeed-search-tag-face                        :foreground zcyan)
     (elfeed-search-title-face                      :foreground zwhite1)
     (elfeed-search-unread-count-face               :foreground zblue)
     (elfeed-search-unread-title-face               :foreground zblue :weight bold :height 1.1)

;;;; elfeed inherited
     (message-cited-text                            :inherit mu4e-cited-1-face)
     (message-header-cc                             :foreground zblue)
     (message-header-name                           :foreground zcyan)
     (message-header-newsgroups                     :foreground zblue)
     (message-header-other                          :foreground zblue)
     (message-header-subject                        :foreground zblue)
     (message-header-to                             :foreground zblue)
     (message-header-xheader                        :inherit link)
     (message-mml                                   :inherit mu4e-header-key-face)
     (message-separator                             :inherit mu4e-compose-separator-face)
                                                    

;;;; mu4e
     (mu4e-cited-1-face                             :foreground zblue :slant normal)
     (mu4e-cited-2-face                             :foreground zcyan :slant normal)
     (mu4e-cited-3-face                             :foreground zcyan :slant normal)
     (mu4e-cited-4-face                             :foreground zblue :slant normal)
     (mu4e-replied-face                             :foreground zblue)
     (mu4e-header-face                              :foreground zwhite1)
     (mu4e-header-key-face                          :foreground zcyan)
     (mu4e-special-header-value-face                :foreground zblue)
     (mu4e-compose-header-face                      :foreground zcyan :slant normal)
     (mu4e-compose-separator-face                   :foreground zgreen :slant normal)
     (mu4e-highlight-face                           :inherit highlight)
     (mu4e-unread-face                              :foreground zcyan :weight bold :height 1.1)
     (mu4e-flagged-face                             :foreground zgreen :weight bold :height 1.1)
     (mu4e-header-highlight-face                    :inherit highlight :weight bold)
     (mu4e-contact-face                             :foreground zblue)

;;;; auctex
      (font-latex-bold-face                         :foreground zwhite1 :weight bold)
      (font-latex-doctex-documentation-face         :background zgrey2)
      (font-latex-italic-face                       :foreground zcyan)
      (font-latex-math-face                         :foreground zcyan)
      (font-latex-sectioning-0-face                 :foreground zblue :height 1.5 :weight bold)
      (font-latex-sectioning-1-face                 :foreground zblue :height 1.4 :weight bold)
      (font-latex-sectioning-2-face                 :foreground zblue :height 1.3 :weight bold)
      (font-latex-sectioning-3-face                 :foreground zblue :height 1.2 :weight bold)
      (font-latex-sectioning-4-face                 :foreground zblue :height 1.1 :weight bold)
      (font-latex-sectioning-5-face                 :foreground zblue :height 1.0 :weight bold)
      (font-latex-sedate-face                       :inherit font-lock-keyword-face)
      (font-latex-string-face                       :foreground zblue)
      (font-latex-verbatim-face                     :foreground zgreen)
      (font-latex-warning-face                      :foreground zgreen)
      (font-latex-script-char-face                  :foreground nil :inherit font-latex-math-face)
      (font-latex-subscript-face                    :inherit font-latex-math-face :height 1)
      (font-latex-superscript-face                  :inherit font-latex-math-face :height 1)

;;;; company-mode
      (company-tooltip                              :background zgrey3 :inherit default)
      (company-scrollbar-bg                         :background zwhite1)
      (company-scrollbar-fg                         :background zgrey4)
      (company-tooltip-annotation                   :foreground zgreen)
      (company-tooltip-common                       :inherit font-lock-constant-face)
      (company-tooltip-selection                    :background zgrey1 :inherit font-lock-function-name-face)
      (company-preview-common                       :inherit secondary-selection)

;;;; csv-mode
      (csv-separator-face                           :foreground zgreen)

;;;; diff-mode
      (diff-added                                   :foreground zcyan)
      (diff-changed                                 :foreground zgreen)
      (diff-removed                                 :foreground zgreen)
      (diff-header                                  :background zgrey3)
      (diff-file-header                             :background zgrey1)
      (diff-hunk-header                             :foreground zgreen :background zgrey3)

;;;; dired
      (dired-marked                                 :foreground zblue :weight bold)

;;;; dired+
      (diredp-compressed-file-suffix                :foreground zblue)
      (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
      (diredp-dir-priv                              :foreground zcyan :background nil)
      (diredp-exec-priv                             :foreground zblue :background nil)
      (diredp-executable-tag                        :foreground zgreen :background nil)
      (diredp-file-name                             :foreground zblue)
      (diredp-file-suffix                           :foreground zcyan)
      (diredp-flag-mark-line                        :background nil :inherit highlight)
      (diredp-ignored-file-name                     :foreground zgrey4)
      (diredp-link-priv                             :foreground zgreen :background nil)
      (diredp-mode-line-flagged                     :foreground zgreen)
      (diredp-mode-line-marked                      :foreground zcyan)
      (diredp-no-priv                               :background nil)
      (diredp-number                                :foreground zblue)
      (diredp-other-priv                            :foreground zgreen :background nil)
      (diredp-rare-priv                             :foreground zgreen :background nil)
      (diredp-read-priv                             :foreground zcyan :background nil)
      (diredp-symlink                               :foreground zgreen)
      (diredp-write-priv                            :foreground zblue :background nil)

;;;; eldoc-mode
      (eldoc-highlight-function-argument            :foreground zblue :weight bold)

;;;; erc
      (erc-direct-msg-face                          :foreground zgreen)
      (erc-error-face                               :foreground zgreen)
      (erc-header-face                              :foreground zwhite1 :background zgrey4)
      (erc-input-face                               :foreground zcyan)
      (erc-keyword-face                             :foreground zblue)
      (erc-current-nick-face                        :foreground zcyan)
      (erc-my-nick-face                             :foreground zcyan)
      (erc-nick-default-face                        :foreground zgreen :weight normal)
      (erc-nick-msg-face                            :foreground zblue :weight normal)
      (erc-notice-face                              :foreground zgrey4)
      (erc-pal-face                                 :foreground zgreen)
      (erc-prompt-face                              :foreground zblue)
      (erc-timestamp-face                           :foreground zcyan)

;;;; eshell
      (eshell-ls-archive                            :foreground zgreen)
      (eshell-ls-backup                             :foreground zgreen)
      (eshell-ls-clutter                            :foreground zgreen)
      (eshell-ls-directory                          :foreground zblue)
      (eshell-ls-executable                         :foreground zcyan)
      (eshell-ls-missing                            :foreground zgreen)
      (eshell-ls-product                            :foreground zgreen)
      (eshell-ls-readonly                           :foreground zwhite1)
      (eshell-ls-special                            :foreground zgreen)
      (eshell-ls-symlink                            :foreground zcyan)
      (eshell-ls-unreadable                         :foreground zgrey4)
      (eshell-prompt                                :foreground zwhite1)

;;;; flycheck-mode
      (flycheck-error                               :underline (:style wave :color zgreen))
      (flycheck-info                                :underline (:style wave :color zcyan))
      (flycheck-warning                             :underline (:style wave :color zgreen))

;;;; flymake-mode
      (flymake-warnline                             :background zgrey3 :underline zgreen)
      (flymake-errline                              :background zgrey3 :underline zgreen)

;;;; flyspell-mode
      (flyspell-duplicate                           :underline (:style wave :color zgreen))
      (flyspell-incorrect                           :underline (:style wave :color zgreen))

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
      (gnus-header-from                             :foreground zgreen :weight bold :inherit message-header-other-face)
      (gnus-header-name                             :inherit message-header-name)
      (gnus-button                                  :foreground nil :inherit link)
      (gnus-signature                               :inherit font-lock-comment-face)

      (gnus-summary-normal-unread                   :foreground zblue :weight normal)
      (gnus-summary-normal-read                     :foreground zwhite1 :weight normal)
      (gnus-summary-normal-ancient                  :foreground zcyan :weight normal)
      (gnus-summary-normal-ticked                   :foreground zgreen :weight normal)
      (gnus-summary-low-unread                      :foreground zgrey4 :weight normal)
      (gnus-summary-low-read                        :foreground zgrey4 :weight normal)
      (gnus-summary-low-ancient                     :foreground zgrey4 :weight normal)
      (gnus-summary-high-unread                     :foreground zblue :weight normal)
      (gnus-summary-high-read                       :foreground zcyan :weight normal)
      (gnus-summary-high-ancient                    :foreground zcyan :weight normal)
      (gnus-summary-high-ticked                     :foreground zgreen :weight normal)
      (gnus-summary-cancelled                       :foreground zgreen :background nil :weight normal)

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
      (helm-action                                  :foreground zwhite1)
      (helm-buffer-directory                        :foreground zgrey4 :background nil :weight bold)
      (helm-buffer-file                             :foreground zcyan)
      (helm-buffer-not-saved                        :foreground zgreen)
      (helm-buffer-process                          :foreground zgrey2)
      (helm-buffer-saved-out                        :foreground zgreen)
      (helm-buffer-size                             :foreground zgreen)
      (helm-candidate-number                        :foreground zblack :background zgreen)
      (helm-ff-directory                            :foreground zgrey4 :background nil :weight bold)
      (helm-ff-executable                           :foreground zcyan)
      (helm-ff-file                                 :foreground zcyan)
      (helm-ff-invalid-symlink                      :foreground zblack :background zgreen)
      (helm-ff-prefix                               :foreground nil :background nil)
      (helm-ff-symlink                              :foreground zblack :background zcyan)
      (helm-grep-cmd-line                           :foreground zcyan)
      (helm-grep-file                               :foreground zcyan)
      (helm-grep-finish                             :foreground zblack :background zgreen)
      (helm-grep-lineno                             :foreground zgrey2)
      (helm-grep-match                              :foreground zblue)
      (helm-grep-running                            :foreground zgreen)
      (helm-header                                  :foreground zblue :background zblack :underline nil)
      (helm-match                                   :foreground zblue)
      (helm-moccur-buffer                           :foreground zcyan)
      (helm-selection                               :foreground nil :background zgrey1 :underline nil)
      (helm-selection-line                          :foreground nil :background zgrey1)
      (helm-separator                               :foreground zgrey1)
      (helm-source-header                           :foreground zwhite1 :background zgrey3 :weight bold)
      (helm-visible-mark                            :foreground zblack :background zcyan)

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
      (ivy-current-match                            :foreground zcyan :background nil :height 1.1 :underline t)
      (ivy-minibuffer-match-face-1                  :foreground zgreen)
      (ivy-minibuffer-match-face-2                  :foreground zblue)
      (ivy-minibuffer-match-face-3                  :foreground zcyan)
      (ivy-minibuffer-match-face-4                  :foreground zcyan)
      (ivy-confirm-face                             :foreground zcyan)
      (ivy-match-required-face                      :foreground zgreen)
      (ivy-virtual                                  :foreground zgrey4)
      (ivy-action                                   :foreground zblue)
      (ivy-prompt-match                             :foreground zgreen :background nil)
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
      (markdown-pre-face                            :foreground zgreen)
      (markdown-inline-code-face                    :foreground nil :inherit markdown-pre-face)

;;;; org-mode
      (org-block-begin-line                         :box (:line-width 1 :color zgrey1) :foreground zgrey3)
      (org-block-end-line                           :inherit org-block-begin-line)
      (org-agenda-structure                         :foreground zgreen)
      (org-agenda-date                              :foreground zblue :underline nil)
      (org-agenda-done                              :foreground zcyan)
      (org-agenda-dimmed-todo-face                  :foreground zgrey4)
      (org-block                                    :foreground zwhite1) ;; "#afafaf")
      (org-code                                     :foreground zgreen)
      (org-column                                   :background zgrey1)
      (org-column-title                             :weight bold :underline t :inherit org-column)
      (org-date                                     :foreground zgreen :underline t)
      (org-document-info                            :foreground zcyan)
      (org-document-info-keyword                    :foreground zcyan)
      (org-document-title                           :foreground zgreen :weight bold :height 1.44)
      (org-done                                     :foreground zcyan)
      (org-meta-line                                :foreground nil :inherit font-lock-comment-face :slant normal)
      (org-ellipsis                                 :foreground zgrey4)
      (org-footnote                                 :foreground zcyan)
      (org-formula                                  :foreground zgreen)
      (org-hide                                     :foreground zgrey2)
      (org-link                                     :foreground zblue)
      (org-scheduled                                :foreground zcyan)
      (org-scheduled-previously                     :foreground zgreen)
      (org-scheduled-today                          :foreground zcyan)
      (org-special-keyword                          :foreground zgreen)
      (org-table                                    :foreground zgreen)
      (org-todo                                     :foreground zgreen)
      (org-upcoming-deadline                        :foreground zgreen)
      (org-warning                                  :foreground zgreen :weight bold)
      (org-level-1                                  :foreground zblue :height 1.3 :weight bold)
      (org-level-2                                  :foreground zcyan :height 1.2 :weight bold)
      (org-level-3                                  :foreground zcyan :height 1.1 :weight bold)
      (org-level-4                                  :foreground zblue :height 1.0 :weight bold)
      (org-level-5                                  :foreground zgreen :height 1.0 :weight bold)
      (org-level-6                                  :foreground zgreen :height 1.0 :weight bold)
      (org-level-7                                  :foreground zgreen :height 1.0 :weight bold)
      (org-level-8                                  :foreground zgreen :height 1.0 :weight bold)

;;;; rainbow-delimiters
      (rainbow-delimiters-depth-1-face              :foreground zgreen)
      (rainbow-delimiters-depth-2-face              :foreground zblue)
      (rainbow-delimiters-depth-3-face              :foreground zcyan)
      (rainbow-delimiters-depth-4-face              :foreground zcyan)
      (rainbow-delimiters-depth-5-face              :foreground zblue)
      (rainbow-delimiters-depth-6-face              :foreground zgreen)
      (rainbow-delimiters-depth-7-face              :foreground zgreen)
      (rainbow-delimiters-depth-8-face              :foreground zgrey2)
      (rainbow-delimiters-depth-9-face              :foreground zwhite1)
      (rainbow-delimiters-mismatched-face           :foreground pureblack :background zblue :weight bold)
      (rainbow-delimiters-unmatched-face            :inherit rainbow-delimiters-mismatched-face)

;;;; sh-mode
      (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
      (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
      (show-paren-match                             :foreground pureblack :background zblue)
      (show-paren-mismatch                          :foreground pureblack :background zgreen)

;;;; MISC IDK
      (mm-command-output                            :foreground zcyan)
      (homoglyph                                    :foreground zcyan) ;; for unrecognized chars
      (nobreak-hyphen                               :foreground zcyan)
      (nobreak-space                                :foreground zcyan :underline t)
      (outline-1                                    :foreground zblue :height 1.3)
      (outline-2                                    :foreground zcyan :height 1.2)
      (outline-3                                    :foreground zcyan :height 1.1)
      (outline-4                                    :foreground zblue :height 1.0)
      (outline-5                                    :foreground zgreen :height 1.0)
      (outline-6                                    :foreground zgreen :height 1.0)
      (outline-7                                    :foreground zgreen :height 1.0)
      (outline-8                                    :foreground zgreen :height 1.0)
      (diary                                        :foreground zblue)

;;;; PACKAGES
      (package-name                                 :foreground zcyan :underline t)
      (package-status-built-in                      :foreground zblue)
      (package-status-installed                     :foreground zcyan)
      (package-status-new                           :foreground zblue :weight bold)

;;;; CUSTOMIZE
      (custom-changed                               :foreground zwhite1 :background zblue)
      (custom-button-pressed-unraised               :foreground zblue :underline t)
      (custom-comment-tag                           :foreground zgrey2)
      (custom-comment                               :foreground zgrey3)
      (custom-documentation                         :foreground zwhite1)
      (custom-face-tag                              :foreground zblue :height 1.1)
      (custom-group-subtitle                        :foreground zwhite1 :weight bold)
      (custom-group-tag                             :foreground zblue :height 1.1)
      (custom-group-tag-1                           :foreground zgreen :height 1.2 :weight bold)
      (custom-invalid                               :foreground zblack :background zgreen)
      (custom-modified                              :foreground zwhite1 :background zblue)
      (custom-rogue                                 :background zblack :foreground zgreen)
      (custom-saved                                 :foreground zwhite1 :underline t)
      (custom-set                                   :background zwhite1 :foreground zblue)
      (custom-state                                 :foreground zcyan)
      (custom-themed                                :background zblue :foreground zwhite1)
      (custom-variable-button                       :foreground zwhite1 :underline t)
      (custom-variable-tag                          :foreground zblue :height 1.1)
      (custom-visibility                            :inherit link :height 1.0)

;;;; INFO
      (info-title-1                                 :foreground zblue :height 1.3)
      (info-title-2                                 :foreground zcyan :height 1.2)
      (info-title-3                                 :foreground zcyan :height 1.1)
      (info-title-4                                 :foreground zblue :height 1.1)
    

;;;; EWW
      (eww-invalid-certificate                      :foreground zgreen)
      (eww-valid-certificate                        :foreground zcyan)
      

;;;; slime-mode
      (slime-highlight-edits-face                   :weight bold)
      (slime-repl-input-face                        :weight normal :underline nil)
      (slime-repl-prompt-face                       :foreground zgreen :underline nil :weight bold)
      (slime-repl-result-face                       :foreground zcyan)
      (slime-repl-output-face                       :foreground zblue :background zgrey3)

;;;; term and ansi-term
      (term                                         :foreground zwhite1 :background zblack)
      (term-color-black                             :foreground zgrey1 :background zblack)
      (term-color-white                             :foreground zwhite1 :background zwhite1)
      (term-color-red                               :foreground zgreen :background zgreen)
      (term-color-yellow                            :foreground zblue :background zblue)
      (term-color-green                             :foreground zcyan :background zcyan)
      (term-color-cyan                              :foreground zcyan :background zcyan)
      (term-color-blue                              :foreground zblue :background zblue)
      (term-color-magenta                           :foreground zgreen :background zgreen)

;;;; undo-tree-mode
      (undo-tree-visualizer-default-face            :foreground zwhite1)
      (undo-tree-visualizer-current-face            :foreground zcyan :weight bold)
      (undo-tree-visualizer-active-branch-face      :foreground zgreen)
      (undo-tree-visualizer-register-face           :foreground zblue)

;;;; whitespace-mode
      (whitespace-empty                             :foreground zgreen :background zblue)
      (whitespace-hspace                            :foreground zgrey4 :background zgrey1)
      (whitespace-indentation                       :foreground zgreen :background zgrey1)
      (whitespace-line                              :foreground zgreen :background zgrey1)
      (whitespace-newline                           :foreground zgrey4)
      (whitespace-space                             :foreground zgrey2 :background zgrey1)
      (whitespace-space-after-tab                   :foreground zgreen :background zgrey1)
      (whitespace-space-before-tab                  :foreground zgreen :background zgrey1)
      (whitespace-tab                               :foreground zgrey2 :background zgrey1)
      (whitespace-trailing                          :foreground zblue :background zgrey1)))

   ;; Anything leftover that doesn't fall neatly into a face goes here.
   (let ((zblack (plist-get theme-colors :zblack))
         (zgrey3 (plist-get theme-colors :zgrey3))
         (zgrey1 (plist-get theme-colors :zgrey1))
         (zgrey2 (plist-get theme-colors :zgrey2))
         (zgrey4 (plist-get theme-colors :zgrey4))
         (zwhite1 (plist-get theme-colors :zwhite1))
         (zwhite1 (plist-get theme-colors :zwhite1))
         (zwhite1 (plist-get theme-colors :zwhite1))
         (zgreen (plist-get theme-colors :zgreen))
         (zgreen (plist-get theme-colors :zgreen))
         (zblue (plist-get theme-colors :zblue))
         (zcyan (plist-get theme-colors :zcyan))
         (zcyan (plist-get theme-colors :zcyan))
         (zblue (plist-get theme-colors :zblue))
         (zgreen (plist-get theme-colors :zgreen))
         (zgreen (plist-get theme-colors :zgreen)))))

;;;###autoload
;; (and load-file-name
;;      (boundp 'custom-theme-load-path)
;;      (add-to-list 'custom-theme-load-path
;;                   (file-name-as-directory
;;                    (file-name-directory load-file-name))))

(provide 'sol-theme)

;;; sol-theme.el ends here
