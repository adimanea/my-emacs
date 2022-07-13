;; base16-my-tomorrow-desat-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Chris Kempson (http://chriskempson.com)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-desat-theme)

(defvar base16-my-tomorrow-desat-colors
  '(:base00 "#242525"
    ;; :base00 "#393a3b"
    :base01 "#454648"
    :base02 "#56585b"
    :base03 "#7f7f7f"           ;; #989898 for lighter   -- for COMMENTS
    :base04 "#b8b8b8"           ;; #d1d1d1 for lighter   -- for MODE-LINE
    :base05 "#cbcbcb"           ;; "#afafaf" for darker   -- for TEXT (fg)
    :base06 "#fafafa"           
    :base07 "#ffffff"
    :base08 "#e68989"
    :base09 "#f8b382"
    :base0A "#ffd894"
    :base0B "#d0d78b"
    :base0C "#9cd8d0"
    :base0D "#92b8d8"
    :base0E "#cba8d5"
    :base0F "#bd877b"
    :pureblack "#000000")
  "All colors for Base16 Tomorrow Night are defined here.")

;; Define the theme
(deftheme base16-my-tomorrow-desat)

;; Add all the faces to the theme
(base16-desat-theme-define 'base16-my-tomorrow-desat base16-my-tomorrow-desat-colors)

;; Mark the theme as provided
(provide-theme 'base16-my-tomorrow-desat)

(provide 'base16-my-tomorrow-desat-theme)

;;; base16-my-tomorrow-desat-theme.el ends here
