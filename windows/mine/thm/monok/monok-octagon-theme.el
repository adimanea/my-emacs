;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-octagon-colors
  '(:zblack "#282a3a"
    :zgrey1 "#535763"
    :zgrey2 "#696d77"
    :zgrey3 "#888d94"
    :zgrey4 "#b2b8bd"
    :zwhite "#eaf2f1"
    :zred "#ff657a"
    :zorange "#ff9b5e"
    :zyellow "#ffd76d"
    :zgreen "#bad761"
    :zcyan "#9cd1bb"
    :zblue "#c39ac9"
    :zmagenta "ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Octagon colors are defined here.")

;; Define the theme
(deftheme monok-octagon)

;; Add all the faces to the theme
(monok-theme-define 'monok-octagon monok-octagon-colors)

;; Mark the theme as provided
(provide-theme 'monok-octagon)
(provide 'monok-octagon-theme)
