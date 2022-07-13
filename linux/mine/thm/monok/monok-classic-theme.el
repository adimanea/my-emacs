;; get $pwd/base-theme
(require 'monok-theme)

(defvar monok-classic-colors
  '(:zblack "#272821"
    :zgrey1 "#575847"
    :zgrey2 "#6e7066"
    :zgrey3 "#919288"
    :zgrey4 "#c0c1b5"
    :zwhite "#fdfff1"
    :zred "#f82570"
    :zorange "#fc961f"
    :zyellow "#e4db73"
    :zgreen "#a6e12d"
    :zcyan "#66d9ee"
    :zblue "#ae81ff"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Classic colors are defined here.")

;; Define the theme
(deftheme monok-classic)

;; Add all the faces to the theme
(monok-theme-define 'monok-classic monok-classic-colors)

;; Mark the theme as provided
(provide-theme 'monok-classic)
(provide 'monok-classic-theme)
