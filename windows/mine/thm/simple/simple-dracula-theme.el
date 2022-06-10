;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-dracula-colors
  '(:zbg "#282a36"
    :zfg "#f8f8f2"
    :zred "#ff5555"
    :zkw "#8be9fd"
    :zstr "#f1fa8c"
    :zcom "#6272a4"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Dracula colors are defined here.")

;; Define the theme
(deftheme simple-dracula)

;; Add all the faces to the theme
(simple-theme-define 'simple-dracula simple-dracula-colors)

;; Mark the theme as provided
(provide-theme 'simple-dracula)
(provide 'simple-dracula-theme)
