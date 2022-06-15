;; get $pwd/base-theme
(require 'simple-theme)

(defvar simple-ubuntu-colors
  '(:zbg "#2c001e"
    :zfg "#f1e9ed"
    :zstr "#ad79a8"
    :zred "#e95420"
    :zkw "#f08763"
    :zcom "#806678"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Ubuntu colors are defined here.")

;; Define the theme
(deftheme simple-ubuntu)

;; Add all the faces to the theme
(simple-theme-define 'simple-ubuntu simple-ubuntu-colors)

;; Mark the theme as provided
(provide-theme 'simple-ubuntu)
(provide 'simple-ubuntu-theme)
