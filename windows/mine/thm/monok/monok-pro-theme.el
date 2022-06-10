;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-pro-colors
  '(:zblack "#2d2a2e"
    :zgrey1 "#5b595c"
    :zgrey2 "#727072"
    :zgrey3 "#939293"
    :zgrey4 "#c1c0c0"
    :zwhite "#fcfcfa"
    :zred "#ff6188"
    :zorange "#fc9867"
    :zyellow "#ffd866"
    :zgreen "#a9dc76"
    :zcyan "#78dce8"
    :zblue "#ab9df2"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Pro colors are defined here.")

;; Define the theme
(deftheme monok-pro)

;; Add all the faces to the theme
(monok-theme-define 'monok-pro monok-pro-colors)

;; Mark the theme as provided
(provide-theme 'monok-pro)
(provide 'monok-pro-theme)
