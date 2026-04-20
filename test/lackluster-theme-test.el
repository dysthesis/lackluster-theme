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

(defun lackluster-theme-test--face-plist (theme face)
  "Return the first face plist that THEME defines for FACE."
  (when-let ((entry (seq-find (lambda (setting)
                                (and (eq (car setting) 'theme-face)
                                     (eq (nth 1 setting) face)
                                     (eq (nth 2 setting) theme)))
                              (get theme 'theme-settings))))
    (cdr (car (nth 3 entry)))))

(defun lackluster-theme-test--face-foreground (theme face)
  "Return THEME's foreground for FACE from its stored face spec."
  (plist-get (lackluster-theme-test--face-plist theme face) :foreground))

(defun lackluster-theme-test--palette-color (theme name)
  "Return resolved palette colour NAME for THEME."
  (lackluster-theme--retrieve-palette-value
   name
   (lackluster-theme--palette-value theme)))

(describe "Lackluster Theme Package"

  (before-each
    (add-to-list 'custom-theme-load-path default-directory)
    (setq lackluster-theme-no-bold t)
    (setq lackluster-theme-pure-black-white nil)
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
      (expect (boundp 'lackluster-night-palette) :to-be-truthy))

    (it "can switch the main surface to pure black and white"
      (setq lackluster-theme-pure-black-white t)
      (expect (lackluster-theme-test--palette-color 'lackluster 'bg-main)
              :to-equal "#000000")
      (expect (lackluster-theme-test--palette-color 'lackluster 'bg-solaire)
              :to-equal "#000000")
      (expect (lackluster-theme-test--palette-color 'lackluster 'fg-main)
              :to-equal "#ffffff")))

  (describe "Theme Properties"
    (it "marks every variant as a theme"
      (dolist (theme lackluster-theme-test-variants)
        (load-theme theme t :no-enable)
        (expect (get theme 'theme-feature) :to-be-truthy)))

    (it "defines solaire support faces for themed configurations"
      (load-theme 'lackluster-night t :no-enable)
      (expect (lackluster-theme-test--face-plist 'lackluster-night 'solaire-default-face)
              :to-be-truthy)))

  (describe "Surface Tuning"
    (it "uses the darker modeline background"
      (expect (lackluster-theme-test--palette-color 'lackluster 'bg-mode-line)
              :to-equal "#0f0f0f"))

    (it "keeps completion selection on the darker auxiliary background"
      (expect (lackluster-theme-test--palette-color 'lackluster 'bg-completion)
              :to-equal "#222222"))

    (it "keeps popup buffers on a dark gray distinct from the main buffer"
      (expect (lackluster-theme-test--palette-color 'lackluster 'bg-popup)
              :to-equal "#161616"))

    (it "uses the main surface background for solaire buffers in pure black mode"
      (setq lackluster-theme-pure-black-white t)
      (load-theme 'lackluster t :no-enable)
      (expect (plist-get (lackluster-theme-test--face-plist 'lackluster 'solaire-default-face)
                         :background)
              :to-equal "#000000")))

  (describe "Org Presentation"
    (it "uses muted gray metadata faces"
      (load-theme 'lackluster t :no-enable)
      (let ((dim (lackluster-theme-test--palette-color 'lackluster 'fg-dim)))
        (expect (lackluster-theme-test--face-foreground 'lackluster 'org-document-info-keyword)
                :to-equal dim)
        (expect (lackluster-theme-test--face-foreground 'lackluster 'org-drawer)
                :to-equal dim)
        (expect (lackluster-theme-test--face-foreground 'lackluster 'org-special-keyword)
                :to-equal dim)))

    (it "bolds org titles and headings"
      (load-theme 'lackluster t :no-enable)
      (expect (plist-get (lackluster-theme-test--face-plist 'lackluster 'org-document-title) :weight)
              :to-equal 'bold)
      (expect (plist-get (lackluster-theme-test--face-plist 'lackluster 'org-level-1) :weight)
              :to-equal 'bold)))

  (describe "Version Control Presentation"
    (it "keeps Magit headings muted rather than brightly accented"
      (load-theme 'lackluster t :no-enable)
      (let ((alt (lackluster-theme-test--palette-color 'lackluster 'fg-alt))
            (dim (lackluster-theme-test--palette-color 'lackluster 'fg-dim)))
        (expect (lackluster-theme-test--face-foreground 'lackluster 'magit-section-heading)
                :to-equal alt)
        (expect (lackluster-theme-test--face-foreground 'lackluster 'magit-branch-remote)
                :to-equal dim)
        (expect (lackluster-theme-test--face-foreground 'lackluster 'majutsu-hash)
                :to-equal dim)))

    (it "uses the main foreground for Magit filenames"
      (load-theme 'lackluster t :no-enable)
      (expect (lackluster-theme-test--face-foreground 'lackluster 'magit-diff-file-heading)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'fg-main)))

    (it "overrides Majutsu log columns to use themed faces"
      (defvar majutsu-log-commit-columns nil)
      (setq majutsu-log-commit-columns nil)
      (lackluster-theme--apply-package-settings)
      (expect (plist-get (car majutsu-log-commit-columns) :face) :to-equal 'majutsu-hash)
      (expect (plist-get (nth 6 majutsu-log-commit-columns) :face) :to-equal 'default)))

  (describe "Language Presentation"
    (it "keeps lackluster-night bindings distinct from keywords"
      (load-theme 'lackluster-night t :no-enable)
      (expect (lackluster-theme-test--face-foreground 'lackluster-night 'font-lock-keyword-face)
              :to-equal (lackluster-theme-test--palette-color 'lackluster-night 'fg-alt))
      (expect (lackluster-theme-test--face-foreground 'lackluster-night 'font-lock-variable-name-face)
              :to-equal (lackluster-theme-test--palette-color 'lackluster-night 'variable)))

    (it "adds a Rust rule to keep double-colon delimiters neutral"
      (expect lackluster-theme--rust-extra-font-lock-keywords
              :to-equal '(("(::)" 1 font-lock-delimiter-face prepend)))))

  (describe "Completion Presentation"
    (it "uses lighter gray Orderless matches"
      (load-theme 'lackluster t :no-enable)
      (expect (lackluster-theme-test--face-foreground 'lackluster 'orderless-match-face-0)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'fg-alt))
      (expect (lackluster-theme-test--face-foreground 'lackluster 'orderless-match-face-2)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'fg-intense)))

    (it "uses gray annotation text and a dedicated popup background"
      (load-theme 'lackluster t :no-enable)
      (expect (lackluster-theme-test--face-foreground 'lackluster 'completions-annotations)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'fg-alt))
      (expect (lackluster-theme-test--face-foreground 'lackluster 'marginalia-documentation)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'fg-alt))
      (expect (plist-get (lackluster-theme-test--face-plist 'lackluster 'vertico-posframe) :background)
              :to-equal (lackluster-theme-test--palette-color 'lackluster 'bg-popup))))

  (describe "Baseline Highlighting"
    (it "keeps baseline keywords and calls neutral while accenting sparse syntax"
      (load-theme 'lackluster t :no-enable)
      (let ((default-fg (lackluster-theme-test--palette-color 'lackluster 'fg-main))
            (keyword-fg (lackluster-theme-test--face-foreground 'lackluster 'font-lock-keyword-face))
            (call-fg (lackluster-theme-test--face-foreground 'lackluster 'font-lock-function-call-face))
            (string-fg (lackluster-theme-test--face-foreground 'lackluster 'font-lock-string-face))
            (comment-fg (lackluster-theme-test--face-foreground 'lackluster 'font-lock-comment-face))
            (definition-fg (lackluster-theme-test--face-foreground 'lackluster 'font-lock-function-name-face)))
        (expect keyword-fg :to-equal default-fg)
        (expect call-fg :to-equal default-fg)
        (expect string-fg :not :to-equal default-fg)
        (expect comment-fg :not :to-equal default-fg)
        (expect definition-fg :not :to-equal default-fg)))

    (it "keeps variable and property use faces neutral in the baseline theme"
      (load-theme 'lackluster t :no-enable)
      (let ((default-fg (lackluster-theme-test--palette-color 'lackluster 'fg-main)))
        (expect (lackluster-theme-test--face-foreground 'lackluster 'font-lock-variable-use-face)
                :to-equal default-fg)
        (expect (lackluster-theme-test--face-foreground 'lackluster 'font-lock-property-use-face)
                :to-equal default-fg))))

  (describe "Variant Overlays"
    (it "makes `lackluster-dark' more subdued than the baseline"
      (load-theme 'lackluster t :no-enable)
      (load-theme 'lackluster-dark t :no-enable)
      (let ((base-fn (lackluster-theme-test--face-foreground 'lackluster 'font-lock-function-name-face))
            (base-string (lackluster-theme-test--face-foreground 'lackluster 'font-lock-string-face)))
        (expect (lackluster-theme-test--face-foreground 'lackluster-dark 'font-lock-function-name-face)
                :not :to-equal base-fn)
        (expect (lackluster-theme-test--face-foreground 'lackluster-dark 'font-lock-string-face)
                :not :to-equal base-string)))

    (it "makes `lackluster-hack' colour keywords"
      (load-theme 'lackluster-hack t :no-enable)
      (let ((default-fg (lackluster-theme-test--palette-color 'lackluster-hack 'fg-main)))
        (expect (lackluster-theme-test--face-foreground 'lackluster-hack 'font-lock-keyword-face)
                :not :to-equal default-fg)
        (expect (lackluster-theme-test--face-foreground 'lackluster-hack 'font-lock-type-face)
                :to-equal default-fg)))

    (it "makes `lackluster-mint' colour type faces"
      (load-theme 'lackluster-mint t :no-enable)
      (let ((default-fg (lackluster-theme-test--palette-color 'lackluster-mint 'fg-main)))
        (expect (lackluster-theme-test--face-foreground 'lackluster-mint 'font-lock-type-face)
                :not :to-equal default-fg)
        (expect (lackluster-theme-test--face-foreground 'lackluster-mint 'font-lock-keyword-face)
                :to-equal default-fg)))

    (it "makes `lackluster-night' the loosest syntax variant"
      (load-theme 'lackluster t :no-enable)
      (load-theme 'lackluster-night t :no-enable)
      (let ((base-string (lackluster-theme-test--face-foreground 'lackluster 'font-lock-string-face))
            (default-fg (lackluster-theme-test--palette-color 'lackluster-night 'fg-main)))
        (expect (lackluster-theme-test--face-foreground 'lackluster-night 'font-lock-keyword-face)
                :not :to-equal default-fg)
        (expect (lackluster-theme-test--face-foreground 'lackluster-night 'font-lock-builtin-face)
                :not :to-equal default-fg)
        (expect (lackluster-theme-test--face-foreground 'lackluster-night 'font-lock-string-face)
                :not :to-equal base-string))))

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
