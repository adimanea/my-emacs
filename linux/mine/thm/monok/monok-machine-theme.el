;; get $pwd/base-theme
(require 'monok-theme)

(defvar monok-machine-colors
  '(:zblack "#273136"
    :zgrey1 "#545f62"
    :zgrey2 "#6b7678"
    :zgrey3 "#8b9798"
    :zgrey4 "#b8c4c3"
    :zwhite "#f2fffc"
    :zred "#ff6d7e"
    :zorange "#ffb270"
    :zyellow "#ffed72"
    :zgreen "#a2e57b"
    :zcyan "#7cd5f1"
    :zblue "#baa0f8"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Machine colors are defined here.")

;; Define the theme
(deftheme monok-machine)

;; Add all the faces to the theme
(monok-theme-define 'monok-machine monok-machine-colors)

;; Mark the theme as provided
(provide-theme 'monok-machine)
(provide 'monok-machine-theme)
