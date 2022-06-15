;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === FIXED PITCH FOR ALL PURPOSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'fixed-pitch nil
                    :family "M+ 1m"
                    :inherit 'default)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "M+ 1m"
                    :inherit 'default)
(set-face-attribute 'variable-pitch nil
                    :family "M+ 1m"
                    :inherit 'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === NEVER USE ITALIC COMMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'normal)
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :inherit 'font-lock-comment-face
                    :slant 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === LATEX MODE FACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (progn
              ;; === NO SEPARATE COLOR FOR _ ^
              (set-face-attribute 'font-latex-script-char-face nil
                                  :foreground nil
                                  :inherit 'font-latex-math-face)
              ;; === NO ITALIC FOR VERBATIM
              (set-face-attribute 'font-latex-verbatim-face nil
                                  :slant 'normal)
              ;; === SUBSCRIPTS AND SUPERSCRIPTS NOT SMALLER
              (set-face-attribute 'font-latex-subscript-face nil
                                  :foreground nil
                                  :inherit 'font-latex-math-face
                                  :height 1)
              (set-face-attribute 'font-latex-superscript-face nil
                                  :foreground nil
                                  :inherit 'font-latex-math-face
                                  :height 1)
              ;; === CUSTOM KEYWORDS (\stuff) = KEYWORDS
              (set-face-attribute 'font-latex-sedate-face nil
                                  :foreground nil
                                  :inherit 'font-lock-keyword-face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; === MISC FACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'link nil
                    :inherit 'dired-symlink)
(set-face-attribute 'link-visited nil
                    :inherit 'dired-symlink)
