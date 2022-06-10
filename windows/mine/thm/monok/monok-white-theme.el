;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-white-colors
    '(:zwhite "#272821"
    :zgrey1 "#575847"
    :zgrey2 "#6e7066"
    :zgrey3 "#919288"
    :zgrey4 "#c0c1b5"
    :zblack "#fdfff1"
    :zred "#f82570"
    :zorange "#fc961f"
    :zyellow "#e4db73"
    :zgreen "#a6e12d"
    :zcyan "#66d9ee"
    :zblue "#ae81ff"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok White colors are defined here.")

;; Define the theme
(deftheme monok-white)

;; Add all the faces to the theme
(monok-theme-define 'monok-white monok-white-colors)

;; Mark the theme as provided
(provide-theme 'monok-white)
(provide 'monok-white-theme)
