;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-soll-colors
  '(:zbg "#fdf6e3"
    :zfg "#002b36"
    :zred "#dc322f"
    :zkw "#2aa198"
    :zstr "#859900"
    :zcom "#586e75"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Sol Light colors are defined here.")

;; Define the theme
(deftheme simple-soll)

;; Add all the faces to the theme
(simple-theme-define 'simple-soll simple-soll-colors)

;; Mark the theme as provided
(provide-theme 'simple-soll)
(provide 'simple-soll-theme)
