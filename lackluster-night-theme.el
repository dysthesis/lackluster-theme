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
     '((variable fg-main)
       (constant fg-alt)
       (fnname luster)
       (fncall gray6)
       (property fg-alt)
       (keyword lack)
       (builtin blue)
       (preprocessor blue)
       (string gray8)
       (escape blue)
       (type fg-alt)
       (comment yellow))
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
