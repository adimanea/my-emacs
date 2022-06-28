(setq user-full-name "Adrian Manea")
(setq user-mail-address "adrianmanea@fastmail.fm")

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))			; install manually (M-x package-install)

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'diminish)					; install separately (M-x package-install)
(require 'bind-key)					; to use :bind in use-package
(setq load-prefer-newer t)

;; === RECENT FILE LIST
(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 20
        recentf-max-saved-items 1000)
  :config
  (recentf-load-list)
  :bind
  ("C-c r". recentf-open-files))

(add-hook 'after-init-mode-hook
		  (lambda ()
			(abbrev-mode -1)))


;; when you decide to use it, see here
;; http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html

;; ARev indicator -- auto refresh buffer when updating
(diminish 'auto-revert-mode)

;; backup (tilde files)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; auto-save (# files)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaved/" t)))

;; === GLOBAL AND INTERFACE TWEAKS
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 5)					; fringe size
(setq inhibit-startup-screen t)
(global-font-lock-mode t)
(blink-cursor-mode -1)				; don't blink cursor
(set-default 'cursor-type 'box)		; box, hollow, bar, (bar . n), hbar, (hbar . n), nil
(column-number-mode)				; line number and column number in modeline
(line-number-mode)
(delete-selection-mode 1)			; write over and replace selected text
(show-paren-mode 1)					; highlight matching parens
(setq-default tab-width 4)
(setq indent-tabs-mode nil)			; convert tabs to spaces
(setq-default next-screen-context-lines 10)	; overlap for page-down
(save-place-mode)					; remember position when reopening buffer
(global-hl-line-mode -1)			; highlight current line
(diminish 'eldoc-mode)				; hide ElDoc (new in v26.1)
(global-visual-line-mode)			; soft wrap text without indicators
(diminish 'visual-line-mode)	    ; hide Wrap mode 

(setq debug-on-error t)

;; utf-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Make sure the cursor returns to previous position when scrolling
(setq scroll-conservatively 10000
	  scroll-preserve-screen-position t)

;; bigger file name in buffer menu mode
(setq Buffer-menu-name-wdith 30)

;; === TIME AND DATE IN MODELINE
(setq display-time-format "   %a, %d %b %H:%M")
(setq display-time-default-load-average nil)		; hide CPU load
(display-time)

;; === LINE-NUMBERS
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode treemacs-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === BUILT-IN PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; === SERVER
(use-package server
  :if window-system
  :defer 5
  :config
  (or (server-running-p) (server-mode))
  (add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
              (local-set-key (kbd "C-x k") 'server-edit))))


;; === DIRED STUFF
(setq directory-free-space-args "-kh" ; human-readable free space, 1K block
      dired-listing-switches "-lahGg")
;; don't show group, human-readable space, no owner

;; === CALENDAR
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)
(setq calendar-latitude 44.439663)
(setq calendar-longitude 26.096306)
(setq calendar-location-name "Bucharest, Romania")
(setq calendar-time-zone 120) ; UTC + 2

;; === FLYMAKE (see also flycheck)
;; (use-package flymake
;;   :init
;;   (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
;;   (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Kill current buffer and close its window
;; (defun bjm/kill-this-buffer ()
;;   "Kill the current buffer."
;;   (interactive)
;;   (kill-buffer (current-buffer))
;;   (delete-window))
;; (global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)



;; MELPA
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

;; === EDIT-INDIRECT for code blocks
(use-package edit-indirect
  :defer t)

;; === RAINBOW PARENS
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

;; === MAGIT
;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind
  ("C-x g" . nil)
  ("C-x g" . magit-status)
  :init
  (setq magit-repository-directories
		;; PATH . depth
		;; '(("d:/repos/" . 2)))
		'(("d:/repos/adrianmanea.xyz" . 0)
		  ("d:/repos/criptografie-upb" . 0)
		  ("d:/repos/pvdp-upb" . 0)
		  ("d:/repos/poligon-cristi" . 0)
		  ("d:/repos/mate-facultate" . 0)
		  ("d:/repos/paperplanes" . 0)
		  ("d:/repos/my-emacs" . 0)))
  (setq magit-repolist-columns
		'(("Name" 25 magit-repolist-column-ident ())
		  ("Version" 25 magit-repolist-column-version ())
		  ("L<U" 5 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
		  ("L>U" 5 magit-repolist-column-unpushed-to-upstream ((:right-align t)))
		  ("Path" 99 magit-repolist-column-path ())))
  )

;; === GIT-GUTTER
;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :defer t
  :config
  (custom-set-variables
   '(git-gutter:window-width 2)
   '(git-gutter:modified-sign "~~")
   '(git-gutter:added-sign "++")
   '(git-gutter:deleted-sign "--")
   '(git-gutter:handled-backends '(git))
   )
  (global-git-gutter-mode +1))

;; === R
(use-package ess
  :ensure t
  :defer t
  :config
  (setq ess-use-flymake nil
        ess-eval-visibly-p nil
        ess-use-eldoc nil))

;; (add-hook 'ess-mode-hook
;; 		  (progn
;; 			(define-key inferior-ess-mode-map
;; 			  (kbd "C-c 1")
;; 			  '(lambda ()
;; 				 (interactive)
;; 				 (insert "%>%")
;; 				 ))))

;; === SMARTPARENS
;; https://github.com/Fuco1/smartparens
;; (use-package smartparens
;;   :defer t
;;   :hook (prog-mode . smartparens-mode)
;;   :diminish smartparens-mode
;;   :init
;;   (require 'smartparens-config)
;;   (sp-pair "\{" "\}") ;; latex literal brackets (included by default)
;;   (sp-pair "<#" "#>")
;;   (sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string)
;;   ;; (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
;;   ;; (sp-local-pair 'emacs-lisp-mode "`" "'") ;; adds `' as a local pair in emacs-lisp-mode
;;   (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;;   ;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;; keybinding management -- ZAMANSKY
;;   ;; https://github.com/zamansky/dot-emacs/blob/main/config.el
;;   (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

;;   (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;;   (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;;   (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;;   (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;   (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;;   (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;;   (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;;   (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
;;   (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;;   (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

;;   (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
;;   (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;;   (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
;;   (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

;;   (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
;;   (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

;;   ;; (bind-key "C-M-s"
;;   ;;           (defhydra smartparens-hydra ()
;;   ;;             "Smartparens"
;;   ;;             ("d" sp-down-sexp "Down")
;;   ;;             ("e" sp-up-sexp "Up")
;;   ;;             ("u" sp-backward-up-sexp "Up")
;;   ;;             ("a" sp-backward-down-sexp "Down")
;;   ;;             ("f" sp-forward-sexp "Forward")
;;   ;;             ("b" sp-backward-sexp "Backward")
;;   ;;             ("k" sp-kill-sexp "Kill" :color blue)
;;   ;;             ("q" nil "Quit" :color blue))
;;   ;;           smartparens-mode-map)

;;   (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
;;   (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
;;   (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
;;   (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
;;   (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
;;   (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
;;   (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
;;   (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
;;   (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
;;   (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
;;   (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
;;   (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
;;   (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
;;   (defvar hyp-s-x-map)
;;   (define-prefix-command 'hyp-s-x-map)
;;   (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
;;   (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
;;   (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
;;   (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

;;   (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

;;   (bind-key ";" 'sp-comment emacs-lisp-mode-map)

;;   (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;   ;;;;;;;;;;;;;;;;;;
;;   ;; pair management

;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;   (sp-with-modes 'org-mode
;;     (sp-local-pair "=" "=" :wrap "C-="))

;;   (sp-with-modes 'textile-mode
;;     (sp-local-pair "*" "*")
;;     (sp-local-pair "_" "_")
;;     (sp-local-pair "@" "@"))

;;   (sp-with-modes 'web-mode
;;     (sp-local-pair "{{#if" "{{/if")
;;     (sp-local-pair "{{#unless" "{{/unless"))

;;   ;;; tex-mode latex-mode
;;   (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
;;     (sp-local-tag "i" "\"<" "\">"))

;;   ;;; lisp modes
;;   (sp-with-modes sp--lisp-modes
;;     (sp-local-pair "(" nil
;;                    :wrap "C-("
;;                    :pre-handlers '(my-add-space-before-sexp-insertion)
;;                    :post-handlers '(my-add-space-after-sexp-insertion)))

;;   (defun my-add-space-after-sexp-insertion (id action _context)
;;     (when (eq action 'insert)
;;       (save-excursion
;;         (forward-char (sp-get-pair id :cl-l))
;;         (when (or (eq (char-syntax (following-char)) ?w)
;;                   (looking-at (sp--get-opening-regexp)))
;;           (insert " ")))))

;;   (defun my-add-space-before-sexp-insertion (id action _context)
;;     (when (eq action 'insert)
;;       (save-excursion
;;         (backward-char (length id))
;;         (when (or (eq (char-syntax (preceding-char)) ?w)
;;                   (and (looking-back (sp--get-closing-regexp))
;;                        (not (eq (char-syntax (preceding-char)) ?'))))
;;           (insert " ")))))

;;   ;;; C++
;;   (sp-with-modes '(malabar-mode c++-mode)
;;     (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
;;   (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
;;                                                       ("* ||\n[i]" "RET")))


;;   (sp-local-pair 'js2-mode "/**" "*/" :post-handlers '(("| " "SPC")
;;                                                        ("* ||\n[i]" "RET")))

;;   ;;; PHP
;;   (sp-with-modes '(php-mode)
;;     (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
;;                                                (my-php-handle-docstring "RET")))
;;     (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
;;     (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") my-php-wrap-handler))
;;     (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

;;   (defun my-php-wrap-handler (&rest _ignored)
;;     (save-excursion
;;       (sp-get sp-last-wrapped-region
;;         (goto-char :beg-in)
;;         (unless (looking-at "[ \t]*$")
;;           (newline-and-indent))
;;         (goto-char :end-in)
;;         (beginning-of-line)
;;         (unless (looking-at "[ \t]*}[ \t]*$")
;;           (goto-char :end-in)
;;           (newline-and-indent))
;;         (indent-region :beg-prf :end-suf))))

;;   (defun my-php-handle-docstring (&rest _ignored)
;;     (-when-let (line (save-excursion
;;                        (forward-line)
;;                        (thing-at-point 'line)))
;;       (cond
;;        ;; variable
;;        ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
;;         (let ((var-name (match-string 1 line))
;;               (type ""))
;;           ;; try to guess the type from the constructor
;;           (-when-let (constructor-args (my-php-get-function-args "__construct" t))
;;             (setq type (or (cdr (assoc var-name constructor-args)) "")))
;;           (insert "* @var " type)
;;           (save-excursion
;;             (insert "\n"))))
;;        ((string-match-p "function" line)
;;         (save-excursion
;;           (let ((args (save-excursion
;;                         (forward-line)
;;                         (my-php-get-function-args nil t))))
;;             (--each args
;;               (when (my-php-should-insert-type-annotation (cdr it))
;;                 (insert (format "* @param %s%s\n"
;;                                 (my-php-translate-type-annotation (cdr it))
;;                                 (car it))))))
;;           (let ((return-type (save-excursion
;;                                (forward-line)
;;                                (my-php-get-function-return-type))))
;;             (when (my-php-should-insert-type-annotation return-type)
;;               (insert (format "* @return %s\n" (my-php-translate-type-annotation return-type))))))
;;         (re-search-forward (rx "@" (or "param" "return") " ") nil t))
;;        ((string-match-p ".*class\\|interface" line)
;;         (save-excursion (insert "\n"))
;;         (insert "* ")))
;;       (let ((o (sp--get-active-overlay)))
;;         (indent-region (overlay-start o) (overlay-end o)))))
;;   )

;; === MARKDOWN
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t))

;; GLSL mode
;; https://melpa.org/#/glsl-mode
(use-package glsl-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode)))


;; EGLOT
;; https://github.com/joaotavora/eglot
(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; disable output to minibuffer (eldoc style)
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
)

;; === C++
(setq-default c-basic-offset 4)
(setq c++-tab-always-indent t)
(setq c-indent-level 4)                  ;; Default is 2
;; (use-package rtags
;;   :defer t)

;; === GGTAGS
;; make sure you install GNU Global first
;; https://www.gnu.org/software/global/download.html
;; full setup here https://tuhdo.github.io/c-ide.html
(use-package ggtags
  :defer t
  ;; :hook
  ;; (c-mode . ggtags-mode)
  ;; (c++-mode . ggtags-mode)
  :config
  ;; (add-hook 'c-mode-common-hook
  ;;         (lambda ()
  ;;           (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  ;;             (ggtags-mode 1))))

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package cmake-mode
  :defer t)

(remove-hook 'c++-mode-hook 'abbrev-mode)

;; === F
;; https://github.com/rejeep/f.el
(use-package f
  :defer t)

;; === DAP
(use-package dap-mode
  :defer t)

;; === LSP-MODE & C++ Config
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
(use-package lsp-mode
  :defer t
  :after (f)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; :hook (python-mode . lsp-deferred)
  :commands lsp)

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
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
  ;; (setq lsp-enable-snippet nil)
  ;; (setq lsp-completion-provider :capf)
  )

;; === YAML Mode
(use-package yaml-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

;; === JSON
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :ensure t
  :defer t)

(use-package flymake-easy
  :ensure t
  :defer t)

(use-package flymake-json
  :ensure t
  :defer t
  :hook (json-mode . flymake-json-load)
  )


;; === Jupyter
;; https://github.com/nnicandro/emacs-jupyter
(use-package jupyter
  :defer t)

;; === EIN
;; https://github.com/millejoh/emacs-ipython-notebook
;; (use-package ein
;;   :defer t)

;; === WEB-MODE
;; https://web-mode.org/
(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; === VIMRC Mode
(use-package vimrc-mode
  :defer t)

;; === ORG STUFF
(use-package org
  :ensure t
  :config
  ;; use current window for org src blocks
  (setq org-src-window-setup 'current-window)
  ;; languages to evaluate source code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)        ; gives access to C, C++, D
     (R . t)
     (python . t)
     (emacs-lisp . t)
     )
   )
  )

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

(use-package browse-kill-ring
  :defer t)

;; === IVY
;; Documentation: https://oremacs.com/swiper/#key-bindings
;; more documentation: https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
(ido-mode -1)

;; extras: swiper:		  https://github.com/abo-abo/swiper
;;		   ivy-rich:      https://github.com/Yevgnen/ivy-rich
;;		   ivy-posframe:  https://github.com/tumashu/ivy-posframe

;; disable ido-mode if using ivy
(use-package ivy
  :init
  (setq ivy-count-format "%d/%d ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (ivy-mode 1)
  (diminish 'ivy-mode))

;; === AUCTEX
(use-package auctex
  :defer t
  :ensure t)


(use-package reftex
  :ensure t
  :defer t
  :commands turn-on-reftex
  :diminish reftex-mode
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t)
    (setq reftex-plug-into-auctex t)
    (setq reftex-extra-bindings t)))

;; Turn on RefTeX for AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
(setq reftex-plug-into-AUCTeX t)

;; === WINDOWS-SPECIFIC CONFIG
(if (eq system-type 'windows-nt)
    (progn
	  ;; use sumatra as PDF viewer
      (setq TeX-view-program-list
            '(("Sumatra PDF" ("\"C:/Users/adria/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                              (mode-io-correlate " -forward-search %b %n ") " %o"))))
	  ;; try to make SUPER key useful
	  (setq w32-pass-apps-to-system nil)
	  (setq w32-apps-modifier 'super)
	  ;; less info about files (if emacs is running slow)
	  (setq w32-get-true-file-attributes nil)
	  ))
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-view-program-selection)
     (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))
  )

(setq font-latex-fontify-script nil)
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))

(defun get-bibtex-keys (file)
  (with-current-buffer (find-file-noselect file)
    (mapcar 'car (bibtex-parse-keys))))

(defun LaTeX-add-all-bibitems-from-bibtex ()
  (interactive)
  (mapc 'LaTeX-add-bibitems
        (apply 'append
               (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))


;; === YASNIPPET
(use-package yasnippet
  :ensure t
  :defer t)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (yas-minor-mode)
            (diminish 'yas-minor-mode)
            (add-to-list 'yas-snippet-dirs "~/.emacs.d/mine/snips/")
            (setq yas-indent-line 'auto)
            (yas-reload-all)))

;; === YASNIPPET-SNIPPETS
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :defer t)

;; === ELPY (Python)
(use-package elpy
  :diminish elpy-mode
  :ensure t
  :defer t
  :init
  (elpy-enable)
  (setq python-shell-interpreter "python")
  (setq python-indent-offset '4))

;; === PROJECTILE
;; https://docs.projectile.mx/projectile/installation.html
;; https://github.com/bbatsov/projectile
(use-package ag
  :ensure t
  :defer t)
(use-package projectile
  :defer t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")))


;; use fixed width fonts everywhere
;; shr = simple HTML renderer, defaults to variable width
(setq-default shr-use-fonts 'nil)

;; === COMPANY COMPLETE
;; https://company-mode.github.io/manual/Frontends.html#Tooltip-Frontends
(use-package company
  :diminish company-mode
  :init
  ;; not having to type RET to accept completion
  (load "~/.emacs.d/mine/company-simple-complete.el")
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay
		(lambda () (if (company-in-string-or-comment) nil 0)))
  (setq company-tooltip-idle-delay
		(lambda () (if (company-in-string-or-comment) nil 0)))
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-margin 1)
  (setq company-format-margin-function #'company-text-icons-margin)
  ;; (add-to-list 'company-backends company-yasnippet)
  )

;; fuzzy search in company
;; https://github.com/jcs-elpa/company-fuzzy
;; (use-package company-fuzzy
;;   :ensure company
;;   :diminish company-fuzzy-mode
;;   :hook (company-mode . company-fuzzy-mode))
  ;;  :config
  ;; (setq company-fuzzy-sorting-backend 'flx
		;; company-fuzzy-prefix-on-top nil
		;; company-fuzzy-history-backends '(company-yasnippet)
		;; company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

;; (with-eval-after-load 'company
;;   (define-key company-active-map
;;     (kbd "TAB")
;; 	#'company-complete-selection)
;;     ;; #'company-complete-common-or-cycle)
;;   (define-key company-active-map
;;     (kbd "<tab>")
;; 	#'company-complete-selection)
;;   (define-key company-active-map
;; 	(kbd "\t")
;; 	#'company-complete-selection)
;;     ;; #'company-complete-common-or-cycle)
;;   (define-key company-active-map
;;     (kbd "<backtab>")
;;     (lambda ()
;;       (interactive)
;;       (company-complete-common-or-cycle -1))))
;; For a more user-friendly output of the pre-defined key bindings, utilize
;; `M-x describe-keymap RET company-active-map' or `C-h f RET company-mode.'

;; == POWERSHELL MODE
;; https://github.com/jschaf/powershell.el
(use-package powershell
  :defer t)


;; === FLYCHECK
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode -1)
  )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === CUSTOM FUNCTIONS
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



;; === COPY BUFFER NAME
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; ===RENAME FILE AND BUFFER
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; === SWAP WINDOWS IN SPLIT
(defun win-swap ()
  "Swap windows using buffer-move.el" ;; it seems it's built-in
  (interactive)
  (if
      (null (windmove-find-other-window 'right))
      (buf-move-left)
    (buf-move-right)))

;; === PASTE IN TERMINAL
;; S-Insert by default
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; === RUN BASH
(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
    (shell "*bash*")))

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

;; === NYAN
(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode 1))

;; === THEMES

;; === MONOKAI
;; https://github.com/oneKelvinSmith/monokai-emacs
(use-package monokai-theme
  :defer t)

;; === GRUVBOX
;; https://github.com/greduan/emacs-theme-gruvbox
(use-package gruvbox-theme
  :defer t)

;; === DRACULA
;; https://github.com/dracula/emacs
(use-package dracula-theme
  :defer t)

;; === SANITYINC
(use-package color-theme-sanityinc-tomorrow
  :defer t)

;; === DOOM THEMES
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic nil)
  ;; (load-theme 'doom-vibrant t)
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; improve org mode native fontification
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (doom-modeline-mode -1))

;; === SOLARIZED
(use-package solarized-theme
  :defer t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-scale-markdown-headlines t))

;; === FONTS
(setq-default antialias 'subpixel)

;; (set-frame-font
;;  "Consolas 12")
(set-face-attribute 'default nil
					:family "PragmataPro Mono"
					:height 120
					:weight 'normal)

(set-face-attribute 'fixed-pitch nil
					:family "PragmataPro Mono")

;; :family "FixedSys Excelsior 3.01"
;; "PragmataPro Mono 12")
;; "Fira Code 11")
;; "Iosevka Fixed 12")
;; "Cascadia Code 11")
;; "Jetbrains Mono 11")
;; "iA Writer Mono S 11")
;; "Anonymous Pro 12")

(setq-default line-spacing 3)

(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/simple/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/monok/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/my16/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/sol/")
(setq custom-theme-directory "~/.emacs.d/mine/thm/")

(load-theme 'doom-vibrant t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:handled-backends '(git))
 '(git-gutter:modified-sign "~~")
 '(git-gutter:window-width 2)
 '(package-selected-packages
   '(glsl-mode flymake-json flymake-easy json-mode ag ess git-gutter yasnippet-snippets powershell dracula-theme gruvbox-theme monokai-theme color-theme-sanityinc-tomorrow ggtags rtags smartparens eglot dap-mode dap which-key cmake-mode web-mode treemacs neotree nyan-mode doom-modeline solarized-theme projectile lsp-mode flycheck elfeed edit-indirect jupyter yaml-mode vimrc-mode use-package undo-tree rainbow-delimiters markdown-mode magit ivy elpy doom-themes diminish browse-kill-ring auctex))
 '(warning-suppress-types '((auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
