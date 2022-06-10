;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-monokpro-colors
  '(:zbg "#2d2a2e"
    :zfg "#fcfcfa"
    :zred "#ff6188"
    :zkw "#78dce8"
    :zstr "#a9dc76"
    :zcom "#5b595c"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Monokpro colors are defined here.")

;; Define the theme
(deftheme simple-monokpro)

;; Add all the faces to the theme
(simple-theme-define 'simple-monokpro simple-monokpro-colors)

;; Mark the theme as provided
(provide-theme 'simple-monokpro)
(provide 'simple-monokpro-theme)
