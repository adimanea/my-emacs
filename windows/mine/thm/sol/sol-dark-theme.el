;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "sol-base-theme.el"))

(defvar sol-dark-colors
  '(:zblack "#002b36"
    :zgrey1 "#073642"
    :zgrey2 "#586e75"
    :zgrey3 "#657b83"
    :zgrey4 "#839496"
    :zgrey5 "#93a1a1"
    :zwhite1 "#eee8d5"
    :zwhite2 "#fdf6e3"
    :zred "#dc322f"
    :zorange "#cb4b16"
    :zyellow "#b58900"
    :zgreen "#869900"
    :zcyan "#2aa198"
    :zblue "#268bd2"
    :zmagenta "#d33682"
    :zviolet "#6c71c4"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Sol Dark colors are defined here.")

;; Define the theme
(deftheme sol-dark)

;; Add all the faces to the theme
(sol-theme-define 'sol-dark sol-dark-colors)

;; Mark the theme as provided
(provide-theme 'sol-dark)
(provide 'sol-dark-theme)
