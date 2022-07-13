;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "light-base-theme.el"))

(defvar light-sol-colors
  '(:zblack "#002b36"
    :zgrey1 "#073642"
    :zgrey2 "#586e75"
    :zgrey3 "#657b83"
    :zgrey4 "#839496"
    :zwhite "#fdf6e3"
    :zred "#dc322f"
    :zorange "#cb4b16"
    :zyellow "#b58900"
    :zgreen "#869900"
    :zcyan "#2aa198"
    :zblue "#268bd2"
    :zmagenta "#d33682"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Light Sol colors are defined here.")

;; Define the theme
(deftheme light-sol)

;; Add all the faces to the theme
(light-theme-define 'light-sol light-sol-colors)

;; Mark the theme as provided
(provide-theme 'light-sol)
(provide 'light-sol-theme)
