;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-2-colors
  '(:zbg "#222222"
    :zfg "#fffbe0"
    :zstr "#ccbc89"
    ;; :zfg "#f7f1ff"
    :zred "#e87264"
    :zkw "#8eb8ae"
    ;; :zkw "#8ea8ca"
    :zcom "#6b6a66"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple 1 colors are defined here.")

;; Define the theme
(deftheme simple-2)

;; Add all the faces to the theme
(simple-theme-define 'simple-2 simple-2-colors)

;; Mark the theme as provided
(provide-theme 'simple-2)
(provide 'simple-2-theme)
