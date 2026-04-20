;;; lackluster-mint-theme.el --- Type-oriented Lackluster variant -*- lexical-binding:t -*-

;;; Commentary:
;;
;; `lackluster-mint' adds emphasis to type-oriented syntax while keeping the
;; rest of the Alabaster-like restraint intact.

;;; Code:

(require 'lackluster-theme)

;;;###theme-autoload
(deftheme lackluster-mint
  "Lackluster variant that highlights types more eagerly."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'lackluster)

(eval-and-compile
  (defconst lackluster-mint-palette
    (append
     '((type green)
       (variable blue)
       (name lack)
       (fg-alt gray8))
     lackluster-palette)
    "Palette for `lackluster-mint'."))

(defcustom lackluster-mint-palette-overrides nil
  "Overrides for `lackluster-mint-palette'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(lackluster-theme--define lackluster-mint lackluster-mint-palette lackluster-mint-palette-overrides)

(provide-theme 'lackluster-mint)
(provide 'lackluster-mint-theme)
;;; lackluster-mint-theme.el ends here
