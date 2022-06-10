;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-gruv-colors
  '(:zbg "#1d2021"
    :zfg "#ebdbb2"
    :zred "#cc241d"
    :zkw "#83a598"
    :zstr "#8ec07c"
    :zcom "#928374"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Gruv colors are defined here.")

;; Define the theme
(deftheme simple-gruv)

;; Add all the faces to the theme
(simple-theme-define 'simple-gruv simple-gruv-colors)

;; Mark the theme as provided
(provide-theme 'simple-gruv)
(provide 'simple-gruv-theme)
