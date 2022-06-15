;; get $pwd/base-theme
(require 'simple-theme)

(defvar simple-gruvl-colors
  '(:zbg "#f9f5d7"
    :zfg "#282828"
    :zred "#9d0006"
    :zkw "#458588"
    :zstr "#98971a"
    :zcom "#928374"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Gruvl colors are defined here.")

;; Define the theme
(deftheme simple-gruvl)

;; Add all the faces to the theme
(simple-theme-define 'simple-gruvl simple-gruvl-colors)

;; Mark the theme as provided
(provide-theme 'simple-gruvl)
(provide 'simple-gruvl-theme)
