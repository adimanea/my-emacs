(setq user-full-name "Adrian Manea")
(setq user-mail-address "adrianmanea@fastmail.fm")

;; Increase garbage collection space
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

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

;; when you decide to use it, see here
;; http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html
(add-hook 'after-init-mode-hook
		  (lambda ()
			(abbrev-mode -1)))

;; ARev indicator -- auto refresh buffer when updating
(diminish 'auto-revert-mode)

;; backup (tilde files)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; auto-save (# files)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "autosaved")) t)))
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaved/" t)))
(setq-default auto-save-no-message t)


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
(global-eldoc-mode nil)
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
(setq Buffer-menu-name-width 30)

;; === TIME AND DATE IN MODELINE
(setq display-time-format "   %a, %d %b %H:%M")
(setq display-time-default-load-average nil)		; hide CPU load
(display-time)

;; === LINE-NUMBERS
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode
	eshell-mode
	shell-mode
	term-mode
	ansi-term-mode
	treemacs-mode
	elfeed-search-mode
	elfeed-show-mode
	)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  ;; :version "green"
  )

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
  ;; :defer 5
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

;; Make project.el NOT automatically set parent directories for Git repositories
;; https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/
;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.
(defun zkj-project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (add-hook 'project-find-functions #'zkj-project-override))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === FRAME, WINDOW & BUFFER commands
;; https://github.com/tarsius/fwb-cmds
(use-package fwb-cmds
  :config
  (global-set-key (kbd "C-x 5 0") 'kill-buffer-and-window))
(global-set-key (kbd "C-x 5 n") 'make-frame)

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
  :config
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

;; === MARKDOWN
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t))

;; === GLSL mode
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

;; === EGLOT
;; https://github.com/joaotavora/eglot
(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs
			   '((c++-mode c-mode) . ("clangd"
									  "--compile-commands-dir=./out/build"
									  "--fallback-style=llvm"
                                      ))
			   )
  (add-to-list 'eglot-ignored-server-capabilities  :hoverProvider)
  ;; (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-eglot-help-at-point)
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; disable output to minibuffer (eldoc style)
  ;; (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (setq eglot-stay-out-of '(company yasnippet eldoc-documentation-strategy))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose)
  (fset #'eglot--snippet-expansion-fn #'ignore)
  ;; (advice-add 'eglot-eldoc-function :around
  ;;           (lambda (oldfun)
  ;;             (let ((help (help-at-pt-kbd-string)))
  ;;               (if help (message "%s" help) (funcall oldfun)))))
  (remove-hook 'eglot-managed-mode-hook 'flymake-mode)
  )

;; === ELDOC-BOX
(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  :bind ("C-h ." . eldoc-box-eglot-help-at-point)
  )

;; === C++
(setq-default c-basic-offset 2)
(setq c++-tab-always-indent t)
(setq c-indent-level 2)                  ;; Default is 2

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

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

;;; === CMAKE (syntax highlight)
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode
  :defer t)

(remove-hook 'c++-mode-hook 'abbrev-mode)

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

;; === BROWSE-KILL-RING (clipboard)
(use-package browse-kill-ring
  :defer t)

;; === IVY
;; Documentation: https://oremacs.com/swiper/#key-bindings
;; more documentation: https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
;; disable ido-mode if using ivy
(ido-mode -1)
;; extras: swiper:		  https://github.com/abo-abo/swiper
;;		   ivy-rich:      https://github.com/Yevgnen/ivy-rich
;;		   ivy-posframe:  https://github.com/tumashu/ivy-posframe

(use-package ivy
  :config
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
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/mine/snips/"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (yas-minor-mode)
            (diminish 'yas-minor-mode)
            (add-to-list 'yas-snippet-dirs "~/.emacs.d/mine/snips")
            (setq yas-indent-line 'auto)
            (yas-reload-all)))


;; (add-hook 'c++-mode-hook yas-minor-mode)

;; === YASNIPPET-SNIPPETS
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; (use-package yasnippet-snippets
;;   :defer t)

;; === ELPY (Python)
(use-package elpy
  :diminish elpy-mode
  :ensure t
  :defer t
  :config
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
  :config
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-track-known-projects-automatically nil)
  )

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
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-margin 1)
  (setq company-format-margin-function #'company-text-icons-margin)
  (setq company-selection-wrap-around t)
  )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-<tab>") #'company-complete-selection))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; === FLYCHECK
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode -1)
  )

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
  (setq elfeed-search-title-max-width '200)
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

;; === RUN BASH
(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
    (shell "*bash*")))


;; === NYAN
(use-package nyan-mode
  :ensure t
  :config
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

;; === SOLARIZED
(use-package solarized-theme
  :defer t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-scale-markdown-headlines t)
  (setq solarized-use-more-italic nil)
  (setq x-underline-at-descent-line t)
  )

;; === BASE16-themes
;; https://github.com/base16-project/base16-emacs
(use-package base16-theme
  :defer t
  :config
  (setq base16-theme-distinct-fringe-background t)
  (setq base16-theme-highlight-mode-line 'box)
  )

;; === MODUS THEMES (built in)
(use-package modus-themes
  :defer t
  :bind ("<f5>" . modus-themes-toggle)
  :config
  (setq
   ;; modus-themes-deuteranopia t
   modus-themes-italic-constructs nil
   modus-themes-bold-constructs t
   modus-themes-mixed-fonts nil
   modus-themes-subtle-line-numbers nil
   modus-themes-variable-pitch-headings nil
   modus-themes-variable-pitch-ui nil
   modus-themes-fringes 'nil		; {nil, 'subtle, 'intense}
   modus-themes-mode-line '(accented borderless (padding . 1) (height . 0.9)) ; {3d, moody, borderless, accented}
   )
  )

;; === FONTS
;; use fixed width fonts everywhere
;; shr = simple HTML renderer, defaults to variable width
(setq-default shr-use-fonts 'nil)

;; (set-frame-font
;;  "Consolas 12")
(if (eq system-type 'windows-nt)
	(progn
	(set-face-attribute 'default nil
						:family "Cascadia Mono"
						:height 110
						:weight 'normal))
  (set-face-attribute 'fixed-pitch nil
					  :family "Cascadia Mono"))

(setq-default line-spacing 0)

(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/simple/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/monok/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/my16/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/thm/sol/")
(setq custom-theme-directory "~/.emacs.d/mine/thm/")

(load-theme 'modus-vivendi t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elfeed eldoc-box fwb-cmds yaml-mode web-mode use-package solarized-theme rtags rainbow-delimiters projectile nyan-mode monokai-theme modus-themes markdown-mode magit json-mode ivy gruvbox-theme glsl-mode ggtags flymake-json flycheck elpy eglot edit-indirect dracula-theme doom-themes diminish color-theme-sanityinc-tomorrow cmake-mode browse-kill-ring base16-theme auctex ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

