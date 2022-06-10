;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-3-colors
  '(:zbg "#f7f5d3"
    :zfg "#3b3a38"
    :zred "#e87264"
    :zkw "#584bdd"
    :zstr "#cc6d31"
    :zcom "#438000"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple 3 colors are defined here.")

;; Define the theme
(deftheme simple-3)

;; Add all the faces to the theme
(simple-theme-define 'simple-3 simple-3-colors)

;; Mark the theme as provided
(provide-theme 'simple-3)
(provide 'simple-3-theme)
