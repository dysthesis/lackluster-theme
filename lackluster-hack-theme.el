;;; lackluster-hack-theme.el --- Keyword-leaning Lackluster variant -*- lexical-binding:t -*-

;;; Commentary:
;;
;; `lackluster-hack' keeps the restrained baseline, but allows keywords to
;; carry a little more colour.

;;; Code:

(require 'lackluster-theme)

;;;###theme-autoload
(deftheme lackluster-hack
  "Lackluster variant with lightly tinted keywords."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'lackluster)

(eval-and-compile
  (defconst lackluster-hack-palette
    (append
     '((keyword fg-alt)
        (builtin lack)
        (preprocessor lack)
        (escape blue))
     lackluster-palette)
    "Palette for `lackluster-hack'."))

(defcustom lackluster-hack-palette-overrides nil
  "Overrides for `lackluster-hack-palette'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(lackluster-theme--define lackluster-hack lackluster-hack-palette lackluster-hack-palette-overrides)

(provide-theme 'lackluster-hack)
(provide 'lackluster-hack-theme)
;;; lackluster-hack-theme.el ends here
