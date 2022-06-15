;; get $pwd/base-theme
(require 'simple-theme)

(defvar simple-nord-colors
  '(:zbg "#2e3440"
    :zfg "#eceff4"
    :zred "#bf616a"
    :zkw "#88c0d0"
	:zstr "#ebcb8b"
    ;; :zstr "#a3be8c"
    :zcom "#6d7a96"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Nord colors are defined here.")

;; Define the theme
(deftheme simple-nord)

;; Add all the faces to the theme
(simple-theme-define 'simple-nord simple-nord-colors)

;; Mark the theme as provided
(provide-theme 'simple-nord)
(provide 'simple-nord-theme)
