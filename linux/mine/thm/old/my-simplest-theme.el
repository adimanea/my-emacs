;; my-simplest-theme.el

(deftheme my-simplest
  "My Simplest Theme -- Using basic and minimalist colours.")

(defvar my-simplest-theme-colors-alist
  '(("my-simplest-black"   . "#000000")
    ("my-simplest-dark-gray" . "#1c1c1a")
    ("my-simplest-white" . "#ffffff")
    ("my-simplest-light-gray" . "#d4d4d2")
    ("my-simplest-comment" . "#707070")
    ("my-simplest-green" . "#52e046")
    ("my-simplest-yellow" . "#e6e460")
    ("my-simplest-blue" . "#7679ff")
    ("my-simplest-cyan" . "#58e9ff")
    ("my-simplest-purple" . "#cf90ff")
    ("my-simplest-magenta" . "#ff7ef0")
    ("my-simplest-orange" . "#de9e58")
    ("my-simplest-red" . "#de5858")))

(defmacro my-simplest-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    my-simplest-theme-colors-alist))
     ,@body))

(my-simplest-with-color-variables
 (custom-theme-set-faces
  'my-simplest

  `(default ((t (:foreground ,my-simplest-light-gray :background ,my-simplest-dark-gray))))
  `(bold ((t (:foreground ,my-simplest-white))))
  `(italic ((t (:foreground ,my-simplest-white))))
  `(success ((t (:foreground ,my-simplest-green))))
  `(warning ((t (:foreground ,my-simplest-orange))))
  `(error ((t (:foreground ,my-simplest-red))))
  `(cursor ((t (:foreground ,my-simplest-orange))))
  `(fringe ((t (:background ,my-simplest-dark-gray))))
  `(region ((t (:background ,my-simplest-comment))))
  `(show-paren-match ((t (:background ,my-simplest-yellow :foreground ,my-simplest-black))))


  `(font-lock-builtin-face ((t (:foreground ,my-simplest-green))))
  `(font-lock-comment-face ((t (:foreground ,my-simplest-comment))))
  `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  `(font-lock-function-name-face ((t (:foreground ,my-simplest-green))))
  `(font-lock-keyword-face ((t (:foreground ,my-simplest-yellow))))
  `(font-lock-preprocessor-face ((t (:foreground ,my-simplest-magenta))))
  `(font-lock-string-face ((t (:foreground ,my-simplest-orange))))
  `(font-lock-type-face ((t (:foreground ,my-simplest-yellow))))
  `(font-lock-constant-face ((t (:foreground ,my-simplest-green))))
  `(font-lock-variable-name-face ((t (:foreground ,my-simplest-green))))
  `(font-lock-warning-face ((t (:foreground ,my-simplest-orange))))

  ;; mode-line
  `(mode-line ((t (:background ,my-simplest-dark-gray :foreground ,my-simplest-light-gray))))
  `(mode-line-buffer-id ((t (:foreground ,my-simplest-magenta))))
  `(mode-line-inactive ((t (:background ,my-simplest-black))))


  ;; org-mode
  `(org-block ((t (:foreground ,my-simplest-cyan))))
  `(org-code ((t (:foreground ,my-simplest-cyan))))
  `(org-document-title ((t (:foreground ,my-simplest-orange))))
  `(org-level-1 ((t :foreground ,my-simplest-yellow :height 1.3)))
  `(org-level-2 ((t :foreground ,my-simplest-yellow :height 1.2)))
  `(org-level-3 ((t :foreground ,my-simplest-yellow :height 1.1)))
  `(org-level-4 ((t :foreground ,my-simplest-yellow :height 1.0)))
  `(org-level-5 ((t :foreground ,my-simplest-yellow)))
  `(org-level-6 ((t :foreground ,my-simplest-yellow)))
  `(org-level-7 ((t :foreground ,my-simplest-yellow)))
  `(org-level-8 ((t :foreground ,my-simplest-yellow)))

  ;; latex-mode
  `(font-latex-sectioning-0-face ((t (:foreground ,my-simplest-orange :height 1.3))))
  `(font-latex-sectioning-1-face ((t (:foreground ,my-simplest-orange :height 1.2))))
  `(font-latex-sectioning-2-face ((t (:foreground ,my-simplest-orange :height 1.1))))
  `(font-latex-sectioning-3-face ((t (:foreground ,my-simplest-orange :height 1.0))))
  `(font-latex-sectioning-4-face ((t (:foreground ,my-simplest-orange :height 1.0))))
  `(font-latex-sectioning-5-face ((t (:foreground ,my-simplest-orange :height 1.0))))
  `(font-latex-italic-face ((t (:foreground ,my-simplest-white))))
  `(font-latex-bold-face ((t (:foreground ,my-simplest-white))))
  `(font-latex-math-face ((t (:foreground ,my-simplest-cyan))))
  `(font-latex-warning-face ((t (:foreground ,my-simplest-red))))
  `(font-latex-doctex-preprocessor-face ((t (:foreground ,my-simplest-blue))))
  `(font-latex-sedate-face ((t (:foreground ,my-simplest-yellow))))

  ;; smartparens
  `(sp-show-pair-mismatched-face ((t (:foreground ,my-simplest-red :background ,my-simplest-dark-gray))))
  `(sp-show-pair-match-face ((t (:background ,my-simplest-magenta))))

  
  ))

(provide-theme 'my-simplest)
