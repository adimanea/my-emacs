;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-sold-colors
  '(:zbg "#002b36"
    :zfg "#839496"
    :zred "#dc322f"
    :zkw "#2aa198"
    :zstr "#859900"
    :zcom "#586e75"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Sol Dark colors are defined here.")

;; Define the theme
(deftheme simple-sold)

;; Add all the faces to the theme
(simple-theme-define 'simple-sold simple-sold-colors)

;; Mark the theme as provided
(provide-theme 'simple-sold)
(provide 'simple-sold-theme)
