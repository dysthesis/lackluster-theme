;;; lackluster-theme-test.el --- Buttercup tests for lackluster-theme -*- lexical-binding:t -*-

;;; Commentary:
;; Regression tests for the packaged Lackluster theme variants.

;;; Code:

(require 'buttercup)
(require 'lackluster-theme)

(defconst lackluster-theme-test-variants
  '(lackluster lackluster-dark lackluster-hack lackluster-mint lackluster-night)
  "Theme variants packaged by `lackluster-theme'.")

(defun lackluster-theme-test--load (theme)
  "Disable active themes and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(describe "Lackluster Theme Package"

  (before-each
    (add-to-list 'custom-theme-load-path default-directory)
    (setq lackluster-theme-no-bold t)
    (mapc #'disable-theme custom-enabled-themes))

  (describe "Package Loading"
    (it "loads the main package without errors"
      (expect (require 'lackluster-theme) :not :to-throw))

    (it "provides a customization group"
      (expect (get 'lackluster-theme 'group-documentation) :to-be-truthy)))

  (describe "Theme Variants"
    (it "registers every packaged theme"
      (expect (lackluster-theme--enable-themes) :to-equal lackluster-theme-collection)
      (dolist (theme lackluster-theme-test-variants)
        (expect (custom-available-themes) :to-contain theme)))

    (it "loads every variant without errors"
      (dolist (theme lackluster-theme-test-variants)
        (expect (load-theme theme t :no-enable) :not :to-throw)
        (expect (custom-theme-p theme) :to-be-truthy)))

    (it "loads themes through `lackluster-theme-load-theme'"
      (expect (lackluster-theme-load-theme 'lackluster) :to-equal 'lackluster)
      (expect custom-enabled-themes :to-contain 'lackluster))

    (it "tracks the package collection"
      (expect lackluster-theme-test-variants :to-equal lackluster-theme-collection)))

  (describe "Palettes"
    (it "defines the baseline palette"
      (expect (boundp 'lackluster-palette) :to-be-truthy)
      (let ((palette (symbol-value 'lackluster-palette)))
        (expect (assoc 'string palette) :to-be-truthy)
        (expect (assoc 'comment palette) :to-be-truthy)
        (expect (assoc 'fnname palette) :to-be-truthy)
        (expect (assoc 'keyword palette) :to-be-truthy)))

    (it "defines overlay palettes for every variant"
      (require 'lackluster-dark-theme)
      (require 'lackluster-hack-theme)
      (require 'lackluster-mint-theme)
      (require 'lackluster-night-theme)
      (expect (boundp 'lackluster-dark-palette) :to-be-truthy)
      (expect (boundp 'lackluster-hack-palette) :to-be-truthy)
      (expect (boundp 'lackluster-mint-palette) :to-be-truthy)
      (expect (boundp 'lackluster-night-palette) :to-be-truthy)))

  (describe "Theme Properties"
    (it "marks every variant as a theme"
      (dolist (theme lackluster-theme-test-variants)
        (load-theme theme t :no-enable)
        (expect (get theme 'theme-feature) :to-be-truthy))))

  (describe "Baseline Highlighting"
    (it "keeps baseline keywords and calls neutral while accenting sparse syntax"
      (lackluster-theme-test--load 'lackluster)
      (let ((default-fg (face-foreground 'default nil t))
            (keyword-fg (face-foreground 'font-lock-keyword-face nil t))
            (call-fg (face-foreground 'font-lock-function-call-face nil t))
            (string-fg (face-foreground 'font-lock-string-face nil t))
            (comment-fg (face-foreground 'font-lock-comment-face nil t))
            (definition-fg (face-foreground 'font-lock-function-name-face nil t)))
        (expect keyword-fg :to-equal default-fg)
        (expect call-fg :to-equal default-fg)
        (expect string-fg :not :to-equal default-fg)
        (expect comment-fg :not :to-equal default-fg)
        (expect definition-fg :not :to-equal default-fg)))

    (it "keeps variable and property use faces neutral in the baseline theme"
      (lackluster-theme-test--load 'lackluster)
      (let ((default-fg (face-foreground 'default nil t)))
        (expect (face-foreground 'font-lock-variable-use-face nil t) :to-equal default-fg)
        (expect (face-foreground 'font-lock-property-use-face nil t) :to-equal default-fg))))

  (describe "Variant Overlays"
    (it "makes `lackluster-dark' more subdued than the baseline"
      (lackluster-theme-test--load 'lackluster)
      (let ((base-fn (face-foreground 'font-lock-function-name-face nil t))
            (base-string (face-foreground 'font-lock-string-face nil t)))
        (lackluster-theme-test--load 'lackluster-dark)
        (expect (face-foreground 'font-lock-function-name-face nil t) :not :to-equal base-fn)
        (expect (face-foreground 'font-lock-string-face nil t) :not :to-equal base-string)))

    (it "makes `lackluster-hack' colour keywords"
      (lackluster-theme-test--load 'lackluster-hack)
      (let ((default-fg (face-foreground 'default nil t)))
        (expect (face-foreground 'font-lock-keyword-face nil t) :not :to-equal default-fg)
        (expect (face-foreground 'font-lock-type-face nil t) :to-equal default-fg)))

    (it "makes `lackluster-mint' colour type faces"
      (lackluster-theme-test--load 'lackluster-mint)
      (let ((default-fg (face-foreground 'default nil t)))
        (expect (face-foreground 'font-lock-type-face nil t) :not :to-equal default-fg)
        (expect (face-foreground 'font-lock-keyword-face nil t) :to-equal default-fg)))

    (it "makes `lackluster-night' the loosest syntax variant"
      (lackluster-theme-test--load 'lackluster)
      (let ((base-string (face-foreground 'font-lock-string-face nil t)))
        (lackluster-theme-test--load 'lackluster-night)
        (expect (face-foreground 'font-lock-keyword-face nil t)
                :not :to-equal (face-foreground 'default nil t))
        (expect (face-foreground 'font-lock-builtin-face nil t)
                :not :to-equal (face-foreground 'default nil t))
        (expect (face-foreground 'font-lock-string-face nil t) :not :to-equal base-string))))

  (describe "Typography Customisation"
    (it "avoids bold by default"
      (setq lackluster-theme-no-bold t)
      (expect (lackluster-theme--bold) :to-be nil))

    (it "can opt back into bold emphasis"
      (setq lackluster-theme-no-bold nil)
      (expect (lackluster-theme--bold) :to-equal '(:inherit bold)))

    (it "keeps heading helper output usable with and without bold"
      (setq lackluster-theme-no-bold t)
      (expect (plist-get (lackluster-theme--heading 1) :height) :to-be-truthy)
      (setq lackluster-theme-no-bold nil)
      (expect (plist-get (lackluster-theme--heading 1) :inherit) :to-be-truthy))))

(provide 'lackluster-theme-test)
;;; lackluster-theme-test.el ends here
