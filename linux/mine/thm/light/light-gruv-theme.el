;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "light-base-theme.el"))

(defvar light-gruv-colors
  '(:zblack "#282828"
    :zgrey1 "#3c3836"
    :zgrey2 "#504945"
    :zgrey3 "#665c54"
    :zgrey4 "#7c6f64"
    :zwhite "#fbf1c7"
    :zred "#9d0006"
    :zorange "#af3a03"
    :zyellow "#b57614"
    :zgreen "#79740e"
    :zcyan "#427b58"
    :zblue "#076678"
    :zmagenta "#8f3f71"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Light Gruv colors are defined here.")

;; Define the theme
(deftheme light-gruv)

;; Add all the faces to the theme
(light-theme-define 'light-gruv light-gruv-colors)

;; Mark the theme as provided
(provide-theme 'light-gruv)
(provide 'light-gruv-theme)
