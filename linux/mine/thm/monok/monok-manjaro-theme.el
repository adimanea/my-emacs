;; get $pwd/base-theme
(require 'monok-theme)

(defvar monok-manjaro-colors
  '(:zblack "#222D31"
    :zgrey1 "#585858"
    :zgrey2 "#555555"
    :zgrey3 "#7E807E"
    :zgrey4 "#d8d8d8"
    :zwhite "#f8f8f8"
    :zred "#ab4642"
    :zorange "#8d8f8d"
    :zyellow "#f7ca88"
    :zgreen "#1abb98"
    :zcyan "#7cafc2"
    :zblue "#ba8baf"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Manjaro colors are defined here.")

;; Define the theme
(deftheme monok-manjaro)

;; Add all the faces to the theme
(monok-theme-define 'monok-manjaro monok-manjaro-colors)

;; Mark the theme as provided
(provide-theme 'monok-manjaro)
(provide 'monok-manjaro-theme)
