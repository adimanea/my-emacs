;; === FLYMAKE (see also flycheck)
(use-package flymake
  :init
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; === UNDO-TREE
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :if window-system
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist 
        '(("" . "~/.emacs.d/undos")))
  )

;; === GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure git-gutter-fringe
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :defer t
  :config
  (custom-set-variables
   '(git-gutter:window-width 1)
   '(git-gutter:modified-sign "~")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-")
   '(git-gutter:handled-backends '(git))
   )
  (setq git-gutter:update-interval 0.02)
  )

;; === R
(use-package ess
  :ensure t
  :defer t
  :config
  (setq ess-use-flymake nil
        ess-eval-visibly-p nil
        ess-use-eldoc nil))

(add-hook 'ess-mode-hook
		  (progn
			(define-key inferior-ess-mode-map
			  (kbd "C-c 1")
			  '(lambda ()
				 (interactive)
				 (insert "%>%")
				 ))))

;; === SMARTPARENS
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :defer t
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (sp-pair "\{" "\}") ;; latex literal brackets (included by default)
  (sp-pair "<#" "#>")
  (sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string)
  ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
  ;; (sp-local-pair 'emacs-lisp-mode "`" "'") ;; adds `' as a local pair in emacs-lisp-mode
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinding management -- ZAMANSKY
  ;; https://github.com/zamansky/dot-emacs/blob/main/config.el
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
  (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

  (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
  (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

  (bind-key "C-M-s"
            (defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map)

  (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
  (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
  (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
  (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
  (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
  (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
  (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
  (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
  (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
  (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
  (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
  (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
  (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
  (defvar hyp-s-x-map)
  (define-prefix-command 'hyp-s-x-map)
  (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
  (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
  (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
  (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

  (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

  ;;;;;;;;;;;;;;;;;;
  ;; pair management

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :wrap "C-="))

  (sp-with-modes 'textile-mode
    (sp-local-pair "*" "*")
    (sp-local-pair "_" "_")
    (sp-local-pair "@" "@"))

  (sp-with-modes 'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))

  ;;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  ;;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-("
                   :pre-handlers '(my-add-space-before-sexp-insertion)
                   :post-handlers '(my-add-space-after-sexp-insertion)))

  (defun my-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun my-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp))
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  ;;; C++
  (sp-with-modes '(malabar-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))


  (sp-local-pair 'js2-mode "/**" "*/" :post-handlers '(("| " "SPC")
                                                       ("* ||\n[i]" "RET")))

  ;;; PHP
  (sp-with-modes '(php-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               (my-php-handle-docstring "RET")))
    (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") my-php-wrap-handler))
    (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

  (defun my-php-wrap-handler (&rest _ignored)
    (save-excursion
      (sp-get sp-last-wrapped-region
        (goto-char :beg-in)
        (unless (looking-at "[ \t]*$")
          (newline-and-indent))
        (goto-char :end-in)
        (beginning-of-line)
        (unless (looking-at "[ \t]*}[ \t]*$")
          (goto-char :end-in)
          (newline-and-indent))
        (indent-region :beg-prf :end-suf))))

  (defun my-php-handle-docstring (&rest _ignored)
    (-when-let (line (save-excursion
                       (forward-line)
                       (thing-at-point 'line)))
      (cond
       ;; variable
       ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
        (let ((var-name (match-string 1 line))
              (type ""))
          ;; try to guess the type from the constructor
          (-when-let (constructor-args (my-php-get-function-args "__construct" t))
            (setq type (or (cdr (assoc var-name constructor-args)) "")))
          (insert "* @var " type)
          (save-excursion
            (insert "\n"))))
       ((string-match-p "function" line)
        (save-excursion
          (let ((args (save-excursion
                        (forward-line)
                        (my-php-get-function-args nil t))))
            (--each args
              (when (my-php-should-insert-type-annotation (cdr it))
                (insert (format "* @param %s%s\n"
                                (my-php-translate-type-annotation (cdr it))
                                (car it))))))
          (let ((return-type (save-excursion
                               (forward-line)
                               (my-php-get-function-return-type))))
            (when (my-php-should-insert-type-annotation return-type)
              (insert (format "* @return %s\n" (my-php-translate-type-annotation return-type))))))
        (re-search-forward (rx "@" (or "param" "return") " ") nil t))
       ((string-match-p ".*class\\|interface" line)
        (save-excursion (insert "\n"))
        (insert "* ")))
      (let ((o (sp--get-active-overlay)))
        (indent-region (overlay-start o) (overlay-end o)))))
  )


;; === RTAGS
(use-package rtags
  :defer t)

;; === LSP & co.
;; === F
;; https://github.com/rejeep/f.el
(use-package f
  :defer t)

;; === DAP
(use-package dap-mode
  :defer t)

;; === LSP-MODE & C++ Config
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;; config from here https://github.com/andreyorst/dotfiles/blob/master/.config/emacs/init.el
(use-package lsp-mode
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :auto)
  (lsp-completion-provider :none)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.05)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-fonfigure nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; completion
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind nil)
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil))

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-theme "Iconless"))
(use-package lsp-mode
  :defer t
  :after (f)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (python-mode . lsp-deferred)
  :commands lsp
  )

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(remove-hook 'c-mode-hook 'lsp)
(remove-hook 'c++-mode-hook 'lsp)
(remove-hook 'python-mode 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode)
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-provider :capf)
  )

;; === LSP-UI
(use-package lsp-ui
  :defer t
  ;; :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  )

(remove-hook 'lsp-mode-hook 'lsp-ui-mode)


;; === ELDOC-BOX
;; https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :defer t
  :diminish eldoc-box-mode
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  )



;; === Jupyter
;; https://github.com/nnicandro/emacs-jupyter
(use-package jupyter
  :defer t)

;; === EIN
;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :defer t)

;; === VIMRC Mode
(use-package vimrc-mode
  :defer t)

;; Search inside links
;; from here: https://emacs.stackexchange.com/questions/21208/search-and-replace-invisible-url-link-in-org-mode
(defvar-local org-descriptive-links-isearch nil
  "Backup of the original value `org-descriptive-links' before isearch.")
(defun org-isearch-mode-hook-function ()
  "Save value of `org-descriptive-links' before isearch."
  (setq org-descriptive-links-isearch org-descriptive-links))
(defun org-isearch-mode-end-hook-function ()
  "Restore original value of `org-descriptive-links' after isearch."
  (unless (eq org-descriptive-links-isearch org-descriptive-links)
    (org-toggle-link-display)))
(defun org-mode-isearch-filter (found)
  "Toggle to literal links if match is found within a string that is hidden in the target of descriptive links."
  (when (and found
             (text-property-any (match-beginning 0) (match-end 0) 'invisible 'org-link)
             org-descriptive-links)
    (org-toggle-link-display))
  found)
(defun org-query-replace-isearch-wrapper (oldfun &rest args)
  "Toggle to literal links if match is found within a string that is hidden in the target of descriptive links."
  (org-isearch-mode-hook-function)
  (prog1 (apply oldfun args)
    (org-isearch-mode-end-hook-function)))
(defun org-mode-setup-isearch ()
  "Setup some specials for isearch in org-mode.
1. Toggle descriptive links if necessary."
  (setq-local search-invisible t)
  (add-hook 'isearch-mode-hook 'org-isearch-mode-hook-function)
  (add-hook 'isearch-mode-end-hook 'org-isearch-mode-end-hook-function)
  (add-function :filter-return (local 'isearch-filter-predicate) #'org-mode-isearch-filter)
  (advice-add 'query-replace :around #'org-query-replace-isearch-wrapper)
  (advice-add 'query-replace-regexp :around #'org-query-replace-isearch-wrapper))
(add-hook 'org-mode-hook 'org-mode-setup-isearch)


;; fuzzy search in company
;; https://github.com/jcs-elpa/company-fuzzy
(use-package company-fuzzy
  :ensure company
  :diminish company-fuzzy-mode
  :hook (company-mode . company-fuzzy-mode))
 :config
(setq company-fuzzy-sorting-backend 'flx
	  company-fuzzy-prefix-on-top nil
	  company-fuzzy-history-backends '(company-yasnippet)
	  company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))

(with-eval-after-load 'company
  (define-key company-active-map
    (kbd "TAB")
	#'company-complete-selection)
    ;; #'company-complete-common-or-cycle)
  (define-key company-active-map
    (kbd "<tab>")
	#'company-complete-selection)
  (define-key company-active-map
	(kbd "\t")
	#'company-complete-selection)
    ;; #'company-complete-common-or-cycle)
  (define-key company-active-map
    (kbd "<backtab>")
    (lambda ()
      (interactive)
      (company-complete-common-or-cycle -1))))
;; For a more user-friendly output of the pre-defined key bindings, utilize
;; `M-x describe-keymap RET company-active-map' or `C-h f RET company-mode.'

;; == POWERSHELL MODE
;; https://github.com/jschaf/powershell.el
(use-package powershell
  :defer t)

;; === IRONY MODE (better for C++ & CMake)
;; https://github.com/Sarcasm/irony-mode
(use-package irony
  :defer t)

;; (when (derived-mode-p major-mode 'c++-mode)
;; (ggtags-mode 1)
;; (flycheck-select-checker "c/c++-cppcheck")))

;; === ELFEED
;; more tips and tricks: https://nullprogram.com/blog/2013/11/26/
;; extra-features: https://nullprogram.com/blog/2015/12/03/
;; "s" for search filter
(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-db-directory "~/.emacs.d/elfeed-db")
  ;; put feeds separately
  (load "~/.emacs.d/feeds.el")
  ;; play yt videos with mpv
  ;; taken from here https://www.reddit.com/r/emacs/comments/7usz5q/youtube_subscriptions_using_elfeed_mpv_no_browser/
  (defun elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
          (quality-arg "")
          (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
      (setq quality-val (string-to-number quality-val))
      (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
      (when (< 0 quality-val)
        (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
      (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))
  (defvar elfeed-mpv-patterns
    '("youtu\\.?be")
    "List of regexp to match against elfeed entry link to know
    whether to use mpv to visit the link.")
  (defun elfeed-visit-or-play-with-mpv ()
    "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise.
    See `elfeed-play-with-mpv'."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
          (patterns elfeed-mpv-patterns))
      (while (and patterns (not (string-match (car elfeed-mpv-patterns) (elfeed-entry-link entry))))
        (setq patterns (cdr patterns)))
      (if patterns
          (elfeed-play-with-mpv)
        (if (eq major-mode 'elfeed-search-mode)
            (elfeed-search-browse-url)
          (elfeed-show-visit)))))
  (define-key elfeed-search-mode-map (kbd "v")
    (lambda ()
      (interactive)
      (elfeed-play-with-mpv)))
  (define-key elfeed-show-mode-map (kbd "v")
    (lambda ()
      (interactive)
      (elfeed-play-with-mpv)))
  (defun elfeed-mark-all-as-read ()
    "Mark currently shown articles read"
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (define-key elfeed-search-mode-map (kbd "A") 'elfeed-mark-all-as-read)
  (define-key elfeed-search-mode-map (kbd "R") 'elfeed-search-fetch) ;; refresh
  (setq elfeed-search-title-max-width '100)
  (setq elfeed-search-title-trailing-width '30)       ;; space left for tags & titles
  (setq elfeed-search-title-min-width '10)
  (setq elfeed-search-date-format '("%d %b" 6 :left)) ;; 25 Mar
  (setq elfeed-search-filter "@3-days-ago +unread")   ;; default filter at startup
  (defun elfeed-open-in-ff ()
    "Open link in Firefox rather than eww"
    (interactive)
    (let ((browse-url-generic-program "firefox"))
      (elfeed-show-visit t)))
  (define-key elfeed-show-mode-map (kbd "f") 'elfeed-open-in-ff)
  (defun elfeed-open-in-next ()
    "Open link in Next browser"
    (interactive)
    (let ((browse-url-generic-program "next"))
      (elfeed-show-visit t)))
  (define-key elfeed-show-mode-map (kbd "x") 'elfeed-open-in-next)
  (define-key elfeed-search-mode-map (kbd "t") 'elfeed-search-set-feed-title))
;; DEFAULT SEARCH-MODE KEYBINDINGS
;; (define-key map "q" 'elfeed-search-quit-window)
;; (define-key map "g" 'elfeed-search-update--force)
;; (define-key map "G" 'elfeed-search-fetch)
;; (define-key map (kbd "RET") 'elfeed-search-show-entry)
;; (define-key map "s" 'elfeed-search-live-filter)
;; (define-key map "S" 'elfeed-search-set-filter)
;; (define-key map "b" 'elfeed-search-browse-url)
;; (define-key map "y" 'elfeed-search-yank)
;; (define-key map "u" 'elfeed-search-tag-all-unread)
;; (define-key map "r" 'elfeed-search-untag-all-unread)
;; (define-key map "n" 'next-line)
;; (define-key map "p" 'previous-line)
;; (define-key map "+" 'elfeed-search-tag-all)
;; (define-key map "-" 'elfeed-search-untag-all)

;; === TREEMACS
;; many dependencies...
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
	(setq treemacs-python-executable "C:\\Program Files\\Python310\\python.exe")
	(setq treemacs-text-scale -0.5)
	(treemacs-resize-icons '15)
	)
  )
;; 	(setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;         treemacs-deferred-git-apply-delay        0.5
;;         treemacs-directory-name-transformer      #'identity
;;         treemacs-display-in-side-window          t
;;         treemacs-eldoc-display                   'simple
;;         treemacs-file-event-delay                5000
;;         treemacs-file-extension-regex            treemacs-last-period-regex-value
;;         treemacs-file-follow-delay               0.2
;;         treemacs-file-name-transformer           #'identity
;;         treemacs-follow-after-init               t
;;         treemacs-expand-after-init               t
;;         treemacs-find-workspace-method           'find-for-file-or-pick-first
;;         treemacs-git-command-pipe                ""
;;         treemacs-goto-tag-strategy               'refetch-index
;;         treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
;;         ;; treemacs-hide-dot-git-directory          t
;;         treemacs-indentation                     2
;;         treemacs-indentation-string              " "
;;         treemacs-is-never-other-window           nil
;;         treemacs-max-git-entries                 5000
;;         treemacs-missing-project-action          'ask
;;         treemacs-move-forward-on-expand          nil
;;         treemacs-no-png-images                   nil
;;         treemacs-no-delete-other-windows         t
;;         treemacs-project-follow-cleanup          nil
;;         treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;         treemacs-position                        'left
;;         treemacs-read-string-input               'from-child-frame
;;         treemacs-recenter-distance               0.1
;;         treemacs-recenter-after-file-follow      nil
;;         treemacs-recenter-after-tag-follow       nil
;;         treemacs-recenter-after-project-jump     'always
;;         treemacs-recenter-after-project-expand   'on-distance
;;         treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;         treemacs-show-cursor                     nil
;;         treemacs-show-hidden-files               t
;;         treemacs-silent-filewatch                nil
;;         treemacs-silent-refresh                  nil
;;         treemacs-sorting                         'alphabetic-asc
;;         treemacs-select-when-already-in-treemacs 'move-back
;;         treemacs-space-between-root-nodes        t
;;         treemacs-tag-follow-cleanup              t
;;         treemacs-tag-follow-delay                1.5
;;         treemacs-text-scale                      nil
;;         treemacs-user-mode-line-format           nil
;;         treemacs-user-header-line-format         nil
;;         treemacs-wide-toggle-width               70
;;         treemacs-width                           35
;;         treemacs-width-increment                 1
;;         treemacs-width-is-initially-locked       t
;;         treemacs-workspace-switch-cleanup        nil)

;; 	;; The default width and height of the icons is 22 pixels. If you are
;; 	;; using a Hi-DPI display, uncomment this to double the icon size.
;; 	;;(treemacs-resize-icons 44)

;; 	(treemacs-follow-mode t)
;; 	(treemacs-filewatch-mode t)
;; 	(treemacs-fringe-indicator-mode 'always)

;; 	(pcase (cons (not (null (executable-find "git")))
;; 				 (not (null treemacs-python-executable)))
;;     (`(t . t)
;;      (treemacs-git-mode 'deferred))
;;     (`(t . _)
;;      (treemacs-git-mode 'simple)))

;; 	(treemacs-hide-gitignored-files-mode nil))
;; :bind
;; (:map global-map
;; 		("M-0"       . treemacs-select-window)
;; 		("C-x t 1"   . treemacs-delete-other-windows)
;; 		("C-x t t"   . treemacs)
;; 		("C-x t d"   . treemacs-select-directory)
;; 		("C-x t B"   . treemacs-bookmark)
;; 		("C-x t C-t" . treemacs-find-file)
;; 		("C-x t M-t" . treemacs-find-tag)))

;; === WHICH-KEY
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === MY CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === WRITE IN WRITE-PROTECTED DIRECTORY
(defun find-file-no-sudo ()
  (interactive)
  (find-file (read-file-name
              "Find file: "
              (if (string-match "^/sudo:root@localhost:" default-directory)
                  (substring default-directory 21)
                default-directory))))
(global-set-key (kbd "C-x C-f") 'find-file-no-sudo)

;; === PASTE IN TERMINAL
;; S-Insert by default
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))
