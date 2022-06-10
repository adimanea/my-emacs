;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-monokris-colors
  '(:zbg "#2c2524"
    :zfg "#fff1f3"
    :zred "#fd6883"
    :zkw "#85dacc"
    :zstr "#adda78"
    :zcom "#5b5353"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Monokris colors are defined here.")

;; Define the theme
(deftheme simple-monokris)

;; Add all the faces to the theme
(simple-theme-define 'simple-monokris simple-monokris-colors)

;; Mark the theme as provided
(provide-theme 'simple-monokris)
(provide 'simple-monokris-theme)
