;; get $pwd/base-theme
(load (concat (file-name-directory (buffer-file-name)) "my16-base-theme.el"))

(defvar my16-nord-colors
  '(:zkw "#88c0d0"
    :zorg "#d08770"
    :zcon "#b48ead"
    :zstr "#ebcb8b"
    :zcom "#4c566a"
    :zred "#bf616a"
    :zgrn "#a3be8c"
    :zty "#81a1c1"
	:zfg1 "#eceff4"
	:zfg2 "#e5e9f0"
	:zfg3 "#d8dee9"
	:zbg1 "#2e3440"
	:zbg2 "#3b4252"
	:zbg3 "#434c5e"
	:zxb2 "#8fbcbb"
	:zpre "#5e81ac"
  "All My16 Nord colors are defined here."))

;; Define the theme
(deftheme my16-nord)

;; Add all the faces to the theme
(my16-theme-define 'my16-nord my16-nord-colors)

;; Mark the theme as provided
(provide-theme 'my16-nord)
(provide 'my16-nord-theme)
