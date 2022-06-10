;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "monok-base-theme.el"))

(defvar monok-nord-colors
  '(:zblack "#2E3440"
    :zgrey1 "#3B4252"
    :zgrey2 "#434C5E"
    :zgrey3 "#4C566A"
    :zgrey4 "#D8DEE9"
    :zwhite "#ECEFF4"
    :zred "#BF616A"
    :zorange "#D08770"
    :zyellow "#EBCB8B"
    :zgreen "#A3BE8C"
    :zcyan "#88C0D0"
    :zblue "#81A1C1"
    :zmagenta "#B48EAD"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Monok Nord colors are defined here.")

;; Define the theme
(deftheme monok-nord)

;; Add all the faces to the theme
(monok-theme-define 'monok-nord monok-nord-colors)

;; Mark the theme as provided
(provide-theme 'monok-nord)
(provide 'monok-nord-theme)
