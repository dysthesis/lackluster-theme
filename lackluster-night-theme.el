;;; lackluster-night-theme.el --- Looser Lackluster variant -*- lexical-binding:t -*-

;;; Commentary:
;;
;; `lackluster-night' is the loosest variant in the family.  It still aims for
;; restrained highlighting, but is more willing to tint structural syntax.

;;; Code:

(require 'lackluster-theme)

;;;###theme-autoload
(deftheme lackluster-night
  "Looser, more colourful Lackluster variant."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'lackluster)

(eval-and-compile
  (defconst lackluster-night-palette
    (append
     '((keyword fg-alt)
       (builtin lack)
       (preprocessor lack)
       (string yellow)
       (escape blue)
       (type blue)
       (comment orange))
     lackluster-palette)
    "Palette for `lackluster-night'."))

(defcustom lackluster-night-palette-overrides nil
  "Overrides for `lackluster-night-palette'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(lackluster-theme--define lackluster-night lackluster-night-palette lackluster-night-palette-overrides)

(provide-theme 'lackluster-night)
(provide 'lackluster-night-theme)
;;; lackluster-night-theme.el ends here
