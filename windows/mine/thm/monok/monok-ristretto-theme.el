;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-ristretto-colors
  '(:zblack "#2c2524"
    :zgrey1 "#5b5353"
    :zgrey2 "#72696a"
    :zgrey3 "#948a8b"
    :zgrey4 "#c3b7b8"
    :zwhite "#fff1f3"
    :zred    "#fd6883"
    :zorange "#f38d70"
    :zyellow "#f9cc6c"
    :zgreen "#adda78"
    :zcyan "#85dacc"
    :zblue "#a8a9eb"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Ristretto colors are defined here.")

;; Define the theme
(deftheme monok-ristretto)

;; Add all the faces to the theme
(monok-theme-define 'monok-ristretto monok-ristretto-colors)

;; Mark the theme as provided
(provide-theme 'monok-ristretto)
(provide 'monok-ristretto-theme)
