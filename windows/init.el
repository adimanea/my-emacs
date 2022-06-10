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

(abbrev-mode -1)
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
(fringe-mode 10)					; 10 pixel fringe
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

;; bigger file name in buffer menu mode
(setq Buffer-menu-name-wdith 30)

;; === TIME AND DATE IN MODELINE
(setq display-time-format "   %a, %d %b %H:%M")
(setq display-time-default-load-average nil)		; hide CPU load
(display-time)

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
      dired-listing-switches "-alhGg")
;; don't show group, human-readable space, no owner

;; === CALENDAR
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)
(setq calendar-latitude 44.439663)
(setq calendar-longitude 26.096306)
(setq calendar-location-name "Bucharest, Romania")
(setq calendar-time-zone 120) ; UTC + 2

;; MELPA
(use-package undo-tree
  :ensure t
  :if window-system)

;; === EDIT-INDIRECT for code blocks
(use-package edit-indirect
  :defer t)


;; === RAINBOW PARENS
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

;; === MAGIT
(use-package magit
  :ensure t
  :bind
  ("C-x g" . nil)
  ("C-x g" . magit-status)
  )

;; === MARKDOWN
(use-package markdown-mode
  :ensure t)

;; === YAML Mode
(use-package yaml-mode
  :defer t)

;; === Jupyter
;; https://github.com/nnicandro/emacs-jupyter
(use-package jupyter
  :defer t)

;; === EIN
;; https://github.com/millejoh/emacs-ipython-notebook
;; (use-package ein
;;   :defer t)


;; === VIMRC Mode
(use-package vimrc-mode
  :defer t)

;; === ORG STUFF
(use-package org
  :ensure t
  :config
  ;; use current window for org src blocks
  (setq org-src-window-setup 'current-window))

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

;; sumatra in windows
(if (eq system-type 'windows-nt)
    (progn
      (setq TeX-PDF-mode t)
      (setq TeX-source-correlate-mode t)
      (setq TeX-source-correlate-method 'synctex)
      (setq TeX-view-program-list
            '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                              (mode-io-correlate " -forward-search %b %n ") " %o"))))

      (eval-after-load 'tex
        '(progn
           (assq-delete-all 'output-pdf TeX-view-program-selection)
           (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))
        )))

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

;; === ELPY (Python)
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq python-shell-interpreter "python")
  (setq python-indent-offset 4))

;; use fixed width fonts everywhere
;; shr = simple HTML renderer, defaults to variable width
(setq-default shr-use-fonts 'nil)

;; === COMPANY COMPLETE
;; https://company-mode.github.io/manual/Frontends.html#Tooltip-Frontends
(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay
		(lambda () (if (company-in-string-or-comment) nil 0)))
  (setq company-tooltip-idle-delay
		(lambda () (if (company-in-string-or-comment) nil 0)))
  (setq company-minimum-prefix-length 0)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-margin 3)
  (setq company-format-margin-function #'company-text-icons-margin)
  )

(with-eval-after-load 'company
  (define-key company-active-map
    (kbd "TAB")
    #'company-complete-common-or-cycle)
  (define-key company-active-map
    (kbd "<tab>")
    #'company-complete-common-or-cycle)
  (define-key company-active-map
    (kbd "<backtab>")
    (lambda ()
      (interactive)
      (company-complete-common-or-cycle -1))))

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
  (interactive "sNew name: ")
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

;; === MY PACKAGES
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/monok")
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/old")
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/light")
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/sol")
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/simple")
;; (add-to-list 'custom-theme-load-path "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/my16")

;; (let ((basedir "C:/Users/adria/AppData/Roaming/.emacs.d/mine/thm/"))
;;   (dolist (f (directory-files basedir))
;; 	(if (and (not (or (equal f ".") (equal f "..")))
;; 			 (file-directory-p (concat basedir f)))
;; 		(add-to-list 'custom-theme-load-path (concat basedir f)))))

;; === DOOM THEMES
(use-package doom-themes
  :defer t)

(set-frame-font
 ;; "Consolas 11")
 "PragmataPro Mono 12")
 ;; "Cascadia Code 11")

(setq-default line-spacing 2)

(load-theme 'doom-vibrant t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elfeed edit-indirect jupyter yaml-mode vimrc-mode use-package undo-tree rainbow-delimiters markdown-mode magit ivy elpy doom-themes diminish browse-kill-ring auctex))
 '(warning-suppress-types '((auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
