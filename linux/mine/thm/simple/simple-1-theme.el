;; get $pwd/base-theme

(require 'simple-theme)

(defvar simple-1-colors
  '(:zbg "#1c1f16"
    :zfg "#f5f3a4"
    :zred "#e87264"
    :zkw "#8ea8ca"
    :zstr "#ffe1de"
    :zcom "#6b6a66"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple 1 colors are defined here.")

;; Define the theme
(deftheme simple-1)

;; Add all the faces to the theme
(simple-theme-define 'simple-1 simple-1-colors)

;; Mark the theme as provided
(provide-theme 'simple-1)
(provide 'simple-1-theme)
