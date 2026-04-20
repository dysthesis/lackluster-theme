;;; lackluster-dark-theme.el --- Subdued Lackluster variant -*- lexical-binding:t -*-

;;; Commentary:
;;
;; `lackluster-dark' pushes the baseline Lackluster port further toward
;; monochrome while keeping comments and diagnostics readable.

;;; Code:

(require 'lackluster-theme)

;;;###theme-autoload
(deftheme lackluster-dark
  "Subdued, near-monochrome Lackluster variant."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'lackluster)

(eval-and-compile
  (defconst lackluster-dark-palette
    (append
     '((fg-main gray7)
       (fg-alt gray6)
       (fg-intense gray8)
       (bg-completion gray2)
       (bg-search-current gray7)
       (blue lack)
       (link lack)
       (name lack)
       (prompt lack)
       (variable lack)
       (fnname lack)
       (constant lack)
       (string lack)
       (builtin fg-alt)
       (punctuation gray5))
     lackluster-palette)
    "Palette for `lackluster-dark'."))

(defcustom lackluster-dark-palette-overrides nil
  "Overrides for `lackluster-dark-palette'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(lackluster-theme--define lackluster-dark lackluster-dark-palette lackluster-dark-palette-overrides)

(provide-theme 'lackluster-dark)
(provide 'lackluster-dark-theme)
;;; lackluster-dark-theme.el ends here
