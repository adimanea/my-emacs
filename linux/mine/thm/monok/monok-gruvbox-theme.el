;; get $pwd/base-theme
(require 'monok-theme)

(defvar monok-gruvbox-colors
  '(:zblack "#1d2021"
    :zgrey1 "#282828"
    :zgrey2 "#3c3836"
    :zgrey3 "#504945"
    :zgrey4 "#665c54"
    :zwhite "#fbf1c7"
    :zred "#cc241d"
    :zorange "#d65d0e"
    :zyellow "#fabd2f"
    :zgreen "#98971a"
    :zcyan "#689d6a"
    :zblue "#458588"
    :zmagenta "#b16286"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Gruvbox colors are defined here.")

;; Define the theme
(deftheme monok-gruvbox)

;; Add all the faces to the theme
(monok-theme-define 'monok-gruvbox monok-gruvbox-colors)

;; Mark the theme as provided
(provide-theme 'monok-gruvbox)
(provide 'monok-gruvbox-theme)
