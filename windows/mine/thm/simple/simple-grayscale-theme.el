;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "simple-base-theme.el"))

(defvar simple-grayscale-colors
  '(:zbg "#2e2e2e"
    :zfg "#e6e6e6"
    :zred "#ec93d3"
    :zkw "#93e0e3"
    :zstr "#d0bf8f"
	;; :zstr "#8fb28f"
    :zcom "#868686"
    :pureblack "#000000"
    :purewhite "#ffffff")
  "All Simple Grayscale colors are defined here.")

;; Define the theme
(deftheme simple-grayscale)

;; Add all the faces to the theme
(simple-theme-define 'simple-grayscale simple-grayscale-colors)

;; Mark the theme as provided
(provide-theme 'simple-grayscale)
(provide 'simple-grayscale-theme)
