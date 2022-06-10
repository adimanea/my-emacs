;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-spectrum-colors
  '(:zblack "#222222"
    :zgrey1 "#525053"
    :zgrey2 "#69676c"
    :zgrey3 "#8b888f"
    :zgrey4 "#bab6c0"
    :zwhite "#f7f1ff"
    :zred "#fc618d"
    :zorange "#fd9353"
    :zyellow "#fce566"
    :zgreen "#7bd88f"
    :zcyan "#5ad4e6"
    :zblue "#948ae3"
    :zmagenta "#ff00ff"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Spectrum colors are defined here.")

;; Define the theme
(deftheme monok-spectrum)

;; Add all the faces to the theme
(monok-theme-define 'monok-spectrum monok-spectrum-colors)

;; Mark the theme as provided
(provide-theme 'monok-spectrum)
(provide 'monok-spectrum-theme)
