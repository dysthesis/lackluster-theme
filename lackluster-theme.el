;;; lackluster-theme.el --- Lackluster themes for Emacs -*- lexical-binding:t -*-

;;; Commentary:
;;
;; `lackluster-theme' packages a family of dark Emacs themes that use the
;; Lackluster palette while following the restrained highlighting philosophy
;; described by Nikita Prokopov in:
;; <https://tonsky.me/blog/syntax-highlighting/>.
;;
;; Available themes:
;; - `lackluster'       : strict, sparse baseline
;; - `lackluster-dark'  : subdued, near-monochrome variant
;; - `lackluster-hack'  : slightly more willing to tint keywords
;; - `lackluster-mint'  : highlights type-oriented syntax
;; - `lackluster-night' : the loosest, most colourful variant

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup lackluster-theme nil
  "Restrained dark themes built from the Lackluster palette."
  :group 'faces
  :link '(url-link :tag "Original palette" "https://github.com/slugbyte/lackluster.nvim")
  :prefix "lackluster-theme-")

(defconst lackluster-theme-collection
  '(lackluster lackluster-dark lackluster-hack lackluster-mint lackluster-night)
  "Symbols of all packaged Lackluster theme variants.")

(defcustom lackluster-theme-post-load-hook nil
  "Hook that runs after loading a Lackluster theme."
  :type 'hook
  :group 'lackluster-theme)

(defcustom lackluster-theme-common-palette-overrides nil
  "Palette overrides that apply to every Lackluster theme.

Entries follow the shape of the theme palette alists, where each
entry is a pair of a palette symbol and a string or another symbol.
Per-theme overrides take precedence over this option."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(defcustom lackluster-theme-no-bold t
  "When non-nil, avoid bold text in theme faces.

This matches the original Alabaster philosophy of using colour
sparingly and avoiding extra typographic emphasis."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-pure-black-white nil
  "When non-nil, use a pure black default background and white foreground.

This only affects the main editing surface.  The rest of the palette
keeps the same muted supporting grays and accents."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-heading-scales
  '((0 . 1.20)
    (1 . 1.14)
    (2 . 1.10)
    (3 . 1.07)
    (4 . 1.04)
    (5 . 1.02)
    (6 . 1.00)
    (7 . 1.00)
    (8 . 1.00))
  "Height multipliers for helper heading faces."
  :type '(alist :key-type integer :value-type number)
  :group 'lackluster-theme)

(eval-and-compile
  (defvar lackluster-theme-custom-variables nil
    "Custom variable specifications shared by all Lackluster themes."))

(defun lackluster-theme--retrieve-palette-value (color palette)
  "Resolve COLOR recursively in PALETTE.

Return `unspecified' if COLOR cannot be resolved to a usable theme
value."
  (let ((value (cadr (assoc color palette))))
    (cond
     ((or (stringp value) (eq value 'unspecified))
      value)
     ((and (symbolp value) value)
      (lackluster-theme--retrieve-palette-value value palette))
     (t 'unspecified))))

(defun lackluster-theme--list-enabled-themes ()
  "Return enabled themes whose names start with `lackluster'."
  (seq-filter
   (lambda (theme)
     (string-prefix-p "lackluster" (symbol-name theme)))
   custom-enabled-themes))

(defun lackluster-theme--enable-themes ()
  "Register all packaged Lackluster themes.

This loads the theme definitions with `:no-enable' so they become
available to completion and preview helpers."
  (mapc
   (lambda (theme)
     (unless (memq theme custom-known-themes)
       (load-theme theme :no-confirm :no-enable)))
   lackluster-theme-collection)
  lackluster-theme-collection)

(defun lackluster-theme--list-known-themes ()
  "Return known themes whose names start with `lackluster'."
  (lackluster-theme--enable-themes)
  (seq-filter
   (lambda (theme)
     (string-prefix-p "lackluster" (symbol-name theme)))
   custom-known-themes))

(defun lackluster-theme--current-theme ()
  "Return the first enabled Lackluster theme, if any."
  (car (or (lackluster-theme--list-enabled-themes)
           (lackluster-theme--list-known-themes))))

(defun lackluster-theme--palette-symbol (theme &optional overrides)
  "Return THEME palette symbol.

With optional OVERRIDES, return the symbol that stores per-theme
palette overrides."
  (when-let ((suffix (cond
                      ((and theme overrides) "palette-overrides")
                      (theme "palette"))))
    (intern (format "%s-%s" theme suffix))))

(defun lackluster-theme--palette-value (theme &optional overrides)
  "Return palette alist for THEME.

When OVERRIDES is non-nil, include common and per-theme overrides in
the returned value."
  (let ((base-value (lackluster-theme--apply-global-palette-adjustments
                     (symbol-value (lackluster-theme--palette-symbol theme)))))
    (if overrides
        (append (symbol-value (lackluster-theme--palette-symbol theme :overrides))
                lackluster-theme-common-palette-overrides
                base-value)
      base-value)))

(defun lackluster-theme--current-theme-palette (&optional overrides)
  "Return the current Lackluster palette.

With OVERRIDES, include common and per-theme override values."
  (if-let ((theme (lackluster-theme--current-theme)))
      (if overrides
          (lackluster-theme--palette-value theme :overrides)
        (lackluster-theme--palette-value theme))
    (user-error "No enabled Lackluster theme could be found")))

(defun lackluster-theme--bold ()
  "Return a plist that adds bold inheritance when enabled."
  (unless lackluster-theme-no-bold
    (list :inherit 'bold)))

(defun lackluster-theme--heading (level)
  "Return heading attributes for LEVEL.

Levels are styled conservatively, with only a little size variance.
Boldness follows `lackluster-theme-no-bold'."
  (let ((height (alist-get level lackluster-theme-heading-scales 1.0)))
    (append (lackluster-theme--bold)
            (unless (= height 1.0)
              (list :height height)))))

(defun lackluster-theme--apply-global-palette-adjustments (palette)
  "Return PALETTE after applying package-level display adjustments."
  (if lackluster-theme-pure-black-white
      (append '((bg-main "#000000")
                (fg-main "#ffffff")
                (fg-intense "#ffffff"))
              palette)
    palette))

(eval-and-compile
  (defvar lackluster-theme-faces
    '(
    ;; Basic faces
    `(default ((,c :background ,bg-main :foreground ,fg-main)))
    `(cursor ((,c :background ,cursor)))
    `(fringe ((,c :background ,bg-main :foreground ,fg-fringe)))
    `(header-line ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(tab-bar ((,c :background ,bg-main :foreground ,fg-alt)))
    `(tab-bar-tab ((,c :background ,bg-alt :foreground ,fg-main)))
    `(tab-bar-tab-inactive ((,c :background ,bg-main :foreground ,fg-dim)))
    `(line-number ((,c :foreground ,fg-dim :background ,bg-main)))
    `(line-number-current-line ((,c ,@(lackluster-theme--bold) :foreground ,fg-alt :background ,bg-main)))
    `(hl-line ((,c :background ,bg-hl-line :extend t)))
    `(region ((,c :background ,bg-region :foreground ,fg-region :extend t)))
    `(secondary-selection ((,c :background ,bg-active :foreground ,fg-main :extend t)))
    `(highlight ((,c :background ,bg-hover :foreground ,fg-main)))
    `(match ((,c :background ,bg-search-match :foreground ,fg-main)))
    `(trailing-whitespace ((,c :background ,bg-err)))
    `(isearch ((,c :background ,bg-search-current :foreground ,fg-search)))
    `(lazy-highlight ((,c :background ,bg-search-lazy :foreground ,fg-main)))
    `(minibuffer-prompt ((,c :foreground ,prompt)))
    `(button ((,c :foreground ,link :underline ,border)))
    `(link ((,c :foreground ,link :underline ,border)))
    `(link-visited ((,c :foreground ,link-alt :underline ,border)))
    `(shadow ((,c :foreground ,fg-dim)))
    `(escape-glyph ((,c :foreground ,constant)))
    `(homoglyph ((,c :foreground ,warning)))
    `(error ((,c ,@(lackluster-theme--bold) :foreground ,err)))
    `(warning ((,c ,@(lackluster-theme--bold) :foreground ,warning)))
    `(success ((,c ,@(lackluster-theme--bold) :foreground ,info)))
    `(tooltip ((,c :background ,bg-alt :foreground ,fg-main)))
    `(widget-field ((,c :background ,bg-alt :foreground ,fg-main)))

    ;; Font lock and treesit-facing faces
    `(font-lock-comment-face ((,c :foreground ,comment)))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-doc-face ((,c :foreground ,docstring)))
    `(font-lock-doc-markup-face ((,c :foreground ,comment)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-escape-face ((,c :foreground ,escape)))
    `(font-lock-regexp-face ((,c :foreground ,escape)))
    `(font-lock-regexp-grouping-backslash ((,c :foreground ,escape)))
    `(font-lock-regexp-grouping-construct ((,c :foreground ,escape)))
    `(font-lock-keyword-face ((,c :foreground ,keyword)))
    `(font-lock-builtin-face ((,c :foreground ,builtin)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-function-call-face ((,c :foreground ,fg-main)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-variable-use-face ((,c :foreground ,fg-main)))
    `(font-lock-property-name-face ((,c :foreground ,fg-alt)))
    `(font-lock-property-use-face ((,c :foreground ,fg-main)))
    `(font-lock-type-face ((,c :foreground ,type)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
    `(font-lock-number-face ((,c :foreground ,constant)))
    `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-lock-negation-char-face ((,c :foreground ,punctuation)))
    `(font-lock-operator-face ((,c :foreground ,punctuation)))
    `(font-lock-bracket-face ((,c :foreground ,punctuation)))
    `(font-lock-delimiter-face ((,c :foreground ,punctuation)))
    `(font-lock-punctuation-face ((,c :foreground ,punctuation)))
    `(font-lock-misc-punctuation-face ((,c :foreground ,punctuation)))
    `(font-lock-warning-face ((,c :inherit warning)))

    ;; Mode line and tabs
    `(mode-line ((,c :background ,bg-mode-line :foreground ,fg-mode-line)))
    `(mode-line-inactive ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(mode-line-buffer-id ((,c :foreground ,fg-main)))
    `(mode-line-emphasis ((,c :foreground ,fg-main)))
    `(mode-line-highlight ((,c :inherit highlight)))

    ;; Diffs
    `(diff-added ((,c :background ,bg-added :foreground ,fg-added)))
    `(diff-changed ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(diff-removed ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(diff-context ((,c :foreground ,fg-dim)))
    `(diff-file-header ((,c :background ,bg-alt :foreground ,fg-main)))
    `(diff-header ((,c :foreground ,fg-alt)))
    `(diff-hunk-header ((,c :background ,bg-active :foreground ,fg-main)))
    `(diff-indicator-added ((,c :inherit diff-added)))
    `(diff-indicator-changed ((,c :inherit diff-changed)))
    `(diff-indicator-removed ((,c :inherit diff-removed)))
    `(diff-refine-added ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(diff-refine-changed ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(diff-refine-removed ((,c :background ,bg-removed-refine :foreground ,fg-removed)))

    ;; Helper faces
    `(lackluster-theme-heading-0 ((,c ,@(lackluster-theme--heading 0) :foreground ,blue)))
    `(lackluster-theme-heading-1 ((,c ,@(lackluster-theme--heading 1) :foreground ,blue)))
    `(lackluster-theme-heading-2 ((,c ,@(lackluster-theme--heading 2) :foreground ,blue)))
    `(lackluster-theme-heading-3 ((,c ,@(lackluster-theme--heading 3) :foreground ,blue)))
    `(lackluster-theme-heading-4 ((,c ,@(lackluster-theme--heading 4) :foreground ,blue)))
    `(lackluster-theme-heading-5 ((,c ,@(lackluster-theme--heading 5) :foreground ,blue)))
    `(lackluster-theme-heading-6 ((,c ,@(lackluster-theme--heading 6) :foreground ,blue)))
    `(lackluster-theme-heading-7 ((,c ,@(lackluster-theme--heading 7) :foreground ,blue)))
    `(lackluster-theme-heading-8 ((,c ,@(lackluster-theme--heading 8) :foreground ,blue)))
    `(lackluster-theme-mark-delete ((,c :inherit error :background ,bg-err)))
    `(lackluster-theme-mark-select ((,c :inherit success :background ,bg-info)))
    `(lackluster-theme-mark-other ((,c :inherit warning :background ,bg-warning)))
    `(lackluster-theme-search-current ((,c :background ,bg-search-current :foreground ,fg-search)))
    `(lackluster-theme-search-lazy ((,c :background ,bg-search-lazy :foreground ,fg-main)))
    `(lackluster-theme-search-replace ((,c :background ,bg-search-replace :foreground ,fg-main)))
    `(lackluster-theme-search-rx-group-0 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-main)))
    `(lackluster-theme-search-rx-group-1 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-main)))
    `(lackluster-theme-search-rx-group-2 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-main)))
    `(lackluster-theme-search-rx-group-3 ((,c :background ,bg-search-rx-group-3 :foreground ,fg-main)))
    `(lackluster-theme-search-match ((,c :background ,bg-search-match :foreground ,fg-main)))
    `(lackluster-theme-underline-error ((,c :underline (:style wave :color ,underline-err))))
    `(lackluster-theme-underline-info ((,c :underline (:style wave :color ,underline-info))))
    `(lackluster-theme-underline-warning ((,c :underline (:style wave :color ,underline-warning))))

    ;; Completion UIs
    `(consult-async-split ((,c :inherit warning)))
    `(consult-file ((,c :foreground ,name)))
    `(consult-imenu-prefix ((,c :inherit shadow)))
    `(consult-key ((,c :foreground ,keybind)))
    `(consult-line-number ((,c :inherit shadow)))
    `(consult-line-number-prefix ((,c :inherit shadow)))
    `(consult-separator ((,c :foreground ,border)))

    `(company-echo-common ((,c :foreground ,blue)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit company-echo-common)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-alt)))
    `(company-template-field ((,c :background ,bg-active :foreground ,fg-main)))
    `(company-tooltip ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(company-tooltip-annotation ((,c :foreground ,fg-dim)))
    `(company-tooltip-common ((,c :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,c :strike-through t :foreground ,fg-dim)))
    `(company-tooltip-selection ((,c :background ,bg-completion :foreground ,fg-main)))

    `(corfu-default ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(corfu-current ((,c :background ,bg-completion :foreground ,fg-main)))
    `(corfu-bar ((,c :background ,fg-alt)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-candidate-overlay-face ((,c :inherit shadow)))
    `(corfu-quick1 ((,c :background ,bg-char-0 :foreground ,fg-main)))
    `(corfu-quick2 ((,c :background ,bg-char-1 :foreground ,fg-main)))

    `(vertico-current ((,c :background ,bg-completion :foreground ,fg-main)))
    `(vertico-group-title ((,c :foreground ,name)))
    `(vertico-group-separator ((,c :foreground ,border)))
    `(vertico-quick1 ((,c :background ,bg-char-0 :foreground ,fg-main)))
    `(vertico-quick2 ((,c :background ,bg-char-1 :foreground ,fg-main)))

    `(orderless-match-face-0 ((,c :foreground ,rainbow-0)))
    `(orderless-match-face-1 ((,c :foreground ,rainbow-1)))
    `(orderless-match-face-2 ((,c :foreground ,rainbow-2)))
    `(orderless-match-face-3 ((,c :foreground ,rainbow-3)))

    `(embark-collect-group-title ((,c :foreground ,name)))
    `(embark-keybinding ((,c :foreground ,keybind)))
    `(embark-selected ((,c :inherit lackluster-theme-mark-select)))

    `(marginalia-archive ((,c :foreground ,blue)))
    `(marginalia-char ((,c :foreground ,warning)))
    `(marginalia-date ((,c :foreground ,fg-alt)))
    `(marginalia-documentation ((,c :foreground ,comment)))
    `(marginalia-file-owner ((,c :inherit shadow)))
    `(marginalia-file-priv-exec ((,c :foreground ,green)))
    `(marginalia-file-priv-link ((,c :foreground ,link)))
    `(marginalia-file-priv-no ((,c :inherit shadow)))
    `(marginalia-file-priv-read ((,c :foreground ,green)))
    `(marginalia-file-priv-write ((,c :foreground ,warning)))
    `(marginalia-function ((,c :foreground ,fnname)))
    `(marginalia-key ((,c :foreground ,keybind)))
    `(marginalia-lighter ((,c :inherit shadow)))
    `(marginalia-mode ((,c :foreground ,fg-alt)))
    `(marginalia-modified ((,c :inherit warning)))
    `(marginalia-number ((,c :foreground ,constant)))
    `(marginalia-size ((,c :foreground ,variable)))
    `(marginalia-string ((,c :foreground ,string)))
    `(marginalia-symbol ((,c :foreground ,builtin)))
    `(marginalia-type ((,c :foreground ,type)))
    `(marginalia-value ((,c :inherit shadow)))

    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,keyword)))
    `(which-key-highlighted-command-face ((,c :foreground ,warning)))
    `(which-key-key-face ((,c :foreground ,keybind)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :inherit shadow)))
    `(which-key-separator-face ((,c :inherit shadow)))
    `(which-key-special-key-face ((,c :inherit error)))

    ;; solaire-mode
    `(solaire-default-face ((,c :background ,bg-alt :foreground ,fg-main)))
    `(solaire-fringe-face ((,c :background ,bg-alt :foreground ,fg-fringe)))
    `(solaire-line-number-face ((,c :background ,bg-alt :foreground ,fg-dim)))
    `(solaire-hl-line-face ((,c :background ,bg-active :foreground ,fg-main :extend t)))
    `(solaire-org-hide-face ((,c :foreground ,bg-alt)))
    `(solaire-region-face ((,c :background ,bg-region :foreground ,fg-region :extend t)))
    `(solaire-mode-line-face ((,c :background ,bg-mode-line :foreground ,fg-mode-line)))
    `(solaire-mode-line-active-face ((,c :background ,bg-mode-line :foreground ,fg-mode-line)))
    `(solaire-mode-line-inactive-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(solaire-header-line-face ((,c :background ,bg-alt :foreground ,fg-alt)))

    ;; Diagnostics and code intelligence
    `(flycheck-error ((,c :inherit lackluster-theme-underline-error)))
    `(flycheck-fringe-error ((,c :inherit lackluster-theme-mark-delete)))
    `(flycheck-fringe-info ((,c :inherit lackluster-theme-mark-select)))
    `(flycheck-fringe-warning ((,c :inherit lackluster-theme-mark-other)))
    `(flycheck-info ((,c :inherit lackluster-theme-underline-info)))
    `(flycheck-warning ((,c :inherit lackluster-theme-underline-warning)))
    `(flymake-error ((,c :inherit lackluster-theme-underline-error)))
    `(flymake-error-echo ((,c :inherit error)))
    `(flymake-note ((,c :inherit lackluster-theme-underline-info)))
    `(flymake-note-echo ((,c :inherit success)))
    `(flymake-warning ((,c :inherit lackluster-theme-underline-warning)))
    `(flymake-warning-echo ((,c :inherit warning)))
    `(eglot-mode-line ((,c :foreground ,modeline-info)))
    `(eglot-diagnostic-tag-unnecessary-face ((,c :inherit lackluster-theme-underline-info)))
    `(lsp-face-highlight-read ((,c :background ,bg-hover :foreground ,fg-main)))
    `(lsp-face-highlight-textual ((,c :background ,bg-hover :foreground ,fg-main)))
    `(lsp-face-highlight-write ((,c :background ,bg-active :foreground ,fg-main)))
    `(lsp-lens-face ((,c :foreground ,fg-dim)))
    `(lsp-details-face ((,c :foreground ,fg-dim)))
    `(doom-modeline-lsp-error ((,c :foreground ,modeline-err)))
    `(doom-modeline-lsp-running ((,c :foreground ,fg-alt)))
    `(doom-modeline-lsp-success ((,c :foreground ,modeline-info)))
    `(doom-modeline-lsp-warning ((,c :foreground ,modeline-warning)))

    ;; Version control
    `(magit-bisect-bad ((,c :inherit error)))
    `(magit-bisect-good ((,c :inherit success)))
    `(magit-bisect-skip ((,c :inherit warning)))
    `(magit-blame-dimmed ((,c :inherit shadow)))
    `(magit-blame-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(magit-branch-local ((,c :foreground ,fg-alt)))
    `(magit-branch-remote ((,c :foreground ,fg-dim)))
    `(magit-branch-upstream ((,c :foreground ,fg-dim)))
    `(magit-cherry-equivalent ((,c :foreground ,fg-dim)))
    `(magit-cherry-unmatched ((,c :foreground ,fg-alt)))
    `(magit-diff-added ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(magit-diff-added-indicator ((,c :inherit magit-diff-added)))
    `(magit-diff-added-highlight ((,c :background ,bg-added :foreground ,fg-added)))
    `(magit-diff-base ((,c :background ,bg-changed-faint :foreground ,fg-changed)))
    `(magit-diff-base-heading ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(magit-diff-base-indicator ((,c :inherit magit-diff-base)))
    `(magit-diff-base-highlight ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(magit-diff-conflict-heading ((,c :background ,bg-warning :foreground ,warning)))
    `(magit-diff-conflict-heading-highlight ((,c :background ,bg-active :foreground ,warning)))
    `(magit-diff-context ((,c :inherit shadow)))
    `(magit-diff-context-highlight ((,c :background ,bg-dim :foreground ,fg-main)))
    `(magit-diff-file-heading ((,c :foreground ,fg-alt)))
    `(magit-diff-file-heading-highlight ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(magit-diff-file-heading-selection ((,c :background ,bg-hover :foreground ,fg-main)))
    `(magit-diff-hunk-heading ((,c :background ,bg-alt :foreground ,fg-alt)))
    `(magit-diff-hunk-heading-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(magit-diff-hunk-heading-selection ((,c :background ,bg-hover :foreground ,fg-main)))
    `(magit-diff-lines-boundary ((,c :background ,bg-alt :foreground ,fg-dim)))
    `(magit-diff-lines-heading ((,c :background ,bg-alt :foreground ,warning)))
    `(magit-diff-our ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(magit-diff-our-heading ((,c :background ,bg-alt :foreground ,fg-added)))
    `(magit-diff-our-highlight ((,c :background ,bg-added :foreground ,fg-added)))
    `(magit-diff-our-indicator ((,c :inherit magit-diff-our)))
    `(magit-diff-removed ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(magit-diff-removed-indicator ((,c :inherit magit-diff-removed)))
    `(magit-diff-removed-highlight ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(magit-diff-revision-summary ((,c :foreground ,fg-alt)))
    `(magit-diff-revision-summary-highlight ((,c :background ,bg-alt :foreground ,fg-main)))
    `(magit-diff-their ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(magit-diff-their-heading ((,c :background ,bg-alt :foreground ,fg-removed)))
    `(magit-diff-their-highlight ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(magit-diff-their-indicator ((,c :inherit magit-diff-their)))
    `(magit-diffstat-added ((,c :foreground ,fg-added)))
    `(magit-diffstat-removed ((,c :foreground ,fg-removed)))
    `(magit-diff-whitespace-warning ((,c :inherit warning)))
    `(magit-dimmed ((,c :inherit shadow)))
    `(magit-hash ((,c :foreground ,fg-dim)))
    `(magit-head ((,c :foreground ,fg-alt)))
    `(magit-header-line ((,c :background ,bg-alt :foreground ,fg-main)))
    `(magit-left-margin ((,c :foreground ,fg-dim)))
    `(magit-log-author ((,c :foreground ,fg-alt)))
    `(magit-log-date ((,c :foreground ,fg-dim)))
    `(magit-log-graph ((,c :foreground ,border)))
    `(magit-mode-line-process ((,c :foreground ,modeline-info)))
    `(magit-mode-line-process-error ((,c :foreground ,modeline-err)))
    `(magit-popup-argument ((,c :foreground ,warning)))
    `(magit-popup-disabled-argument ((,c :inherit shadow)))
    `(magit-popup-heading ((,c :foreground ,fg-alt)))
    `(magit-popup-key ((,c :foreground ,keybind)))
    `(magit-popup-option-value ((,c :foreground ,constant)))
    `(magit-process-ng ((,c :inherit error)))
    `(magit-process-ok ((,c :inherit success)))
    `(magit-reflog-amend ((,c :foreground ,fg-alt)))
    `(magit-reflog-checkout ((,c :foreground ,fg-alt)))
    `(magit-reflog-cherry-pick ((,c :foreground ,fg-alt)))
    `(magit-reflog-commit ((,c :foreground ,fg-alt)))
    `(magit-reflog-merge ((,c :foreground ,fg-alt)))
    `(magit-reflog-other ((,c :foreground ,fg-dim)))
    `(magit-reflog-rebase ((,c :foreground ,fg-alt)))
    `(magit-reflog-remote ((,c :foreground ,fg-alt)))
    `(magit-reflog-reset ((,c :inherit error)))
    `(magit-refname ((,c :foreground ,fg-dim)))
    `(magit-section-child-count ((,c :foreground ,fg-dim)))
    `(magit-section-heading ((,c :foreground ,fg-alt)))
    `(magit-section-secondary-heading ((,c :foreground ,fg-dim)))
    `(magit-section-heading-selection ((,c :background ,bg-hover :foreground ,fg-main)))
    `(magit-section-highlight ((,c :background ,bg-alt :extend t)))
    `(magit-sequence-drop ((,c :foreground ,fg-dim)))
    `(magit-sequence-head ((,c :foreground ,fg-alt)))
    `(magit-sequence-part ((,c :foreground ,fg-alt)))
    `(magit-sequence-stop ((,c :foreground ,fg-alt)))
    `(magit-signature-bad ((,c :inherit error)))
    `(magit-signature-error ((,c :inherit error)))
    `(magit-signature-expired ((,c :inherit warning)))
    `(magit-signature-good ((,c :inherit success)))
    `(magit-signature-revoked ((,c :inherit warning)))
    `(magit-signature-untrusted ((,c :inherit warning)))
    `(magit-tag ((,c :foreground ,constant)))

    `(majutsu-annotate-date ((,c :foreground ,fg-dim)))
    `(majutsu-annotate-heading ((,c :background ,bg-active :foreground ,fg-alt :extend t)))
    `(majutsu-annotate-hash ((,c :foreground ,fg-dim)))
    `(majutsu-annotate-highlight ((,c :background ,bg-active :foreground ,fg-main :extend t)))
    `(majutsu-annotate-name ((,c :foreground ,fg-alt)))
    `(majutsu-annotate-summary ((,c :foreground ,fg-main)))
    `(majutsu-conflict-added-face ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(majutsu-conflict-base-face ((,c :background ,bg-warning :foreground ,fg-main)))
    `(majutsu-conflict-context-face ((,c :foreground ,fg-dim)))
    `(majutsu-conflict-marker-face ((,c :foreground ,fg-dim)))
    `(majutsu-conflict-refined-added ((,c :background ,bg-added :foreground ,fg-added)))
    `(majutsu-conflict-refined-removed ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(majutsu-conflict-removed-face ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(majutsu-diff-color-words-focus ((,c :background ,bg-active :extend t)))
    `(majutsu-diffstat-binary ((,c :foreground ,fg-dim)))
    `(majutsu-hash ((,c :foreground ,fg-dim)))
    `(majutsu-interactive-selected-hunk ((,c :background ,bg-active :foreground ,fg-main)))
    `(majutsu-interactive-selected-region ((,c :background ,bg-hover :foreground ,fg-main)))

    `(diff-hl-change ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(diff-hl-delete ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(diff-hl-insert ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(git-gutter:added ((,c :foreground ,fg-added)))
    `(git-gutter:deleted ((,c :foreground ,fg-removed)))
    `(git-gutter:modified ((,c :foreground ,fg-changed)))
    `(git-gutter:unchanged ((,c :foreground ,fg-dim)))
    `(git-gutter-fr:added ((,c :foreground ,fg-added :background ,bg-added)))
    `(git-gutter-fr:deleted ((,c :foreground ,fg-removed :background ,bg-removed)))
    `(git-gutter-fr:modified ((,c :foreground ,fg-changed :background ,bg-changed)))

    ;; Org
    `(org-agenda-calendar-daterange ((,c :foreground ,fg-alt)))
    `(org-agenda-calendar-event ((,c :foreground ,fg-main)))
    `(org-agenda-clocking ((,c :background ,bg-warning :foreground ,warning)))
    `(org-agenda-column-dateline ((,c :background ,bg-alt)))
    `(org-agenda-current-time ((,c :foreground ,fg-main)))
    `(org-agenda-date ((,c :foreground ,blue)))
    `(org-agenda-date-today ((,c :foreground ,blue :underline t)))
    `(org-agenda-date-weekend ((,c :foreground ,fg-alt)))
    `(org-agenda-dimmed-todo-face ((,c :inherit shadow)))
    `(org-agenda-done ((,c :foreground ,fg-dim)))
    `(org-agenda-structure ((,c :foreground ,fg-alt)))
    `(org-archived ((,c :background ,bg-alt :foreground ,fg-main)))
    `(org-block ((,c :background ,bg-inactive :extend t)))
    `(org-block-begin-line ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(org-block-end-line ((,c :inherit org-block-begin-line)))
    `(org-checkbox ((,c :foreground ,warning)))
    `(org-checkbox-statistics-done ((,c :foreground ,fg-dim)))
    `(org-checkbox-statistics-todo ((,c :foreground ,warning)))
    `(org-clock-overlay ((,c :background ,bg-hover)))
    `(org-code ((,c :foreground ,fg-alt)))
    `(org-column ((,c :background ,bg-alt)))
    `(org-column-title ((,c :background ,bg-active :foreground ,fg-main)))
    `(org-date ((,c :foreground ,blue :underline t)))
    `(org-document-info ((,c :foreground ,fg-dim)))
    `(org-document-info-keyword ((,c :foreground ,fg-dim)))
    `(org-document-title ((,c ,@(lackluster-theme--heading 0) :weight bold :foreground ,blue)))
    `(org-done ((,c :foreground ,fg-dim)))
    `(org-drawer ((,c :foreground ,fg-dim)))
    `(org-ellipsis ((,c :foreground ,fg-dim)))
    `(org-footnote ((,c :foreground ,blue :underline t)))
    `(org-formula ((,c :foreground ,warning)))
    `(org-headline-done ((,c :foreground ,fg-dim)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-latex-and-related ((,c :foreground ,lack)))
    `(org-level-1 ((,c ,@(lackluster-theme--heading 1) :weight bold :foreground ,blue)))
    `(org-level-2 ((,c ,@(lackluster-theme--heading 2) :weight bold :foreground ,blue)))
    `(org-level-3 ((,c ,@(lackluster-theme--heading 3) :weight bold :foreground ,blue)))
    `(org-level-4 ((,c ,@(lackluster-theme--heading 4) :weight bold :foreground ,blue)))
    `(org-level-5 ((,c ,@(lackluster-theme--heading 5) :weight bold :foreground ,blue)))
    `(org-level-6 ((,c ,@(lackluster-theme--heading 6) :weight bold :foreground ,blue)))
    `(org-level-7 ((,c ,@(lackluster-theme--heading 7) :weight bold :foreground ,blue)))
    `(org-level-8 ((,c ,@(lackluster-theme--heading 8) :weight bold :foreground ,blue)))
    `(org-link ((,c :foreground ,link :underline t)))
    `(org-list-dt ((,c :foreground ,fg-main)))
    `(org-macro ((,c :foreground ,lack)))
    `(org-meta-line ((,c :foreground ,fg-dim)))
    `(org-mode-line-clock ((,c :inherit mode-line)))
    `(org-mode-line-clock-overrun ((,c :inherit mode-line :foreground ,err)))
    `(org-priority ((,c :foreground ,warning)))
    `(org-property-value ((,c :foreground ,fg-alt)))
    `(org-quote ((,c :inherit org-block)))
    `(org-scheduled ((,c :foreground ,fg-main)))
    `(org-scheduled-previously ((,c :foreground ,warning)))
    `(org-scheduled-today ((,c :foreground ,fg-main)))
    `(org-special-keyword ((,c :foreground ,fg-dim)))
    `(org-table ((,c :foreground ,fg-main)))
    `(org-tag ((,c :foreground ,fg-dim)))
    `(org-target ((,c :foreground ,blue)))
    `(org-time-grid ((,c :foreground ,fg-dim)))
    `(org-todo ((,c :foreground ,warning)))
    `(org-upcoming-deadline ((,c :foreground ,warning)))
    `(org-verbatim ((,c :foreground ,fg-alt)))
    `(org-verse ((,c :inherit org-block)))
    `(org-warning ((,c :inherit warning)))

    ;; Dired
    `(dired-broken-symlink ((,c :inherit (error link))))
    `(dired-directory ((,c :foreground ,blue)))
    `(dired-flagged ((,c :inherit lackluster-theme-mark-delete)))
    `(dired-header ((,c :foreground ,fg-main)))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :foreground ,fg-main)))
    `(dired-marked ((,c :inherit lackluster-theme-mark-select)))
    `(dired-symlink ((,c :foreground ,link)))
    `(dired-warning ((,c :inherit warning)))
    `(diredfl-deletion ((,c :inherit dired-flagged)))
    `(diredfl-dir-name ((,c :inherit dired-directory)))
    `(diredfl-dir-priv ((,c :inherit dired-directory)))
    `(diredfl-flag-mark ((,c :inherit dired-marked)))
    `(diredfl-flag-mark-line ((,c :inherit dired-marked)))
    `(diredfl-symlink ((,c :inherit dired-symlink)))
    `(image-dired-thumb-flagged ((,c :background ,err)))
    `(image-dired-thumb-header-file-name ((,c :foreground ,fg-main)))
    `(image-dired-thumb-header-file-size ((,c :foreground ,info)))
    `(image-dired-thumb-mark ((,c :background ,bg-info :foreground ,info)))

    ;; Navigation and editing helpers
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c :background ,bg-active :foreground ,fg-main)))
    `(avy-lead-face ((,c :background ,bg-char-0 :foreground ,fg-main)))
    `(avy-lead-face-0 ((,c :background ,bg-char-1 :foreground ,fg-main)))
    `(avy-lead-face-1 ((,c :background ,bg-inactive :foreground ,fg-alt)))
    `(avy-lead-face-2 ((,c :background ,bg-char-2 :foreground ,fg-main)))
    `(show-paren-match ((,c :background ,bg-search-current :foreground ,fg-search)))
    `(show-paren-match-expression ((,c :background ,bg-alt :foreground ,fg-main)))
    `(show-paren-mismatch ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(highlight-indentation-face ((,c :background ,bg-dim)))
    `(highlight-symbol-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(highlight-numbers-number ((,c :foreground ,constant)))
    `(yas-field-highlight ((,c :background ,bg-hover :foreground ,fg-main)))

    `(rainbow-delimiters-base-error-face ((,c :inherit show-paren-mismatch)))
    `(rainbow-delimiters-base-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-delimiters-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-delimiters-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-delimiters-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-delimiters-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-delimiters-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-delimiters-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-delimiters-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-delimiters-mismatched-face ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(rainbow-delimiters-unmatched-face ((,c :inherit rainbow-delimiters-mismatched-face)))

    ;; Miscellaneous
    `(all-the-icons-blue ((,c :foreground ,blue)))
    `(all-the-icons-blue-alt ((,c :foreground ,blue)))
    `(all-the-icons-cyan ((,c :foreground ,lack)))
    `(all-the-icons-cyan-alt ((,c :foreground ,lack)))
    `(all-the-icons-dgreen ((,c :foreground ,green)))
    `(all-the-icons-dorange ((,c :foreground ,warning)))
    `(all-the-icons-dred ((,c :foreground ,err)))
    `(all-the-icons-lgreen ((,c :foreground ,green)))
    `(all-the-icons-lorange ((,c :foreground ,warning)))
    `(all-the-icons-lred ((,c :foreground ,err)))
    `(all-the-icons-maroon ((,c :foreground ,lack)))
    `(all-the-icons-red ((,c :foreground ,err)))
    `(all-the-icons-yellow ((,c :foreground ,warning)))
    `(all-the-icons-dired-dir-face ((,c :foreground ,blue)))
    `(helpful-heading ((,c :inherit lackluster-theme-heading-1)))

    `(custom-button ((,c :box (:color ,border :style released-button))))
    `(custom-button-mouse ((,c :inherit (highlight custom-button))))
    `(custom-button-pressed ((,c :inherit (secondary-selection custom-button))))
    `(custom-changed ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(custom-comment ((,c :inherit shadow)))
    `(custom-comment-tag ((,c :inherit shadow)))
    `(custom-face-tag ((,c :foreground ,type)))
    `(custom-group-tag ((,c :foreground ,builtin)))
    `(custom-group-tag-1 ((,c :foreground ,constant)))
    `(custom-invalid ((,c :inherit error :strike-through t)))
    `(custom-modified ((,c :inherit custom-changed)))
    `(custom-rogue ((,c :inherit custom-invalid)))
    `(custom-set ((,c :inherit success)))
    `(custom-state ((,c :foreground ,fg-alt)))
    `(custom-themed ((,c :inherit custom-changed)))
    `(custom-variable-tag ((,c :foreground ,variable)))
      `(custom-variable-obsolete ((,c :inherit shadow))))
    "Face specifications shared by the packaged Lackluster themes."))

(defmacro lackluster-theme--define (name palette &optional overrides faces)
  "Define theme NAME using PALETTE and optional OVERRIDES.

FACES defaults to `lackluster-theme-faces'."
  (declare (indent 0))
  (let* ((palette-symbols (delete-dups (copy-sequence (mapcar #'car (symbol-value palette)))))
         (palette-value (cl-gensym "palette"))
         (face-forms (or (and faces (symbol-value faces))
                         (symbol-value 'lackluster-theme-faces)))
         (variable-forms (symbol-value 'lackluster-theme-custom-variables)))
    `(let* ((c '((class color) (min-colors 256)))
            (,palette-value (lackluster-theme--palette-value ',name ',overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(lackluster-theme--retrieve-palette-value ',color ,palette-value)))
                      palette-symbols))
       (ignore c ,@palette-symbols)
       (apply #'custom-theme-set-faces ',name
              (list ,@face-forms))
       (apply #'custom-theme-set-variables ',name
              (list ,@variable-forms)))))

(defmacro lackluster-theme-with-colors (&rest body)
  "Evaluate BODY with the current Lackluster palette bound."
  (declare (indent 0))
  (let* ((palette-symbols (delete-dups (copy-sequence (mapcar #'car (lackluster-theme--current-theme-palette)))))
         (palette-value (cl-gensym "palette")))
    `(let* ((c '((class color) (min-colors 256)))
            (,palette-value (lackluster-theme--current-theme-palette :overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(lackluster-theme--retrieve-palette-value ',color ,palette-value)))
                      palette-symbols))
       (ignore c ,@palette-symbols)
       ,@body)))

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(defun lackluster-theme--annotate-theme (theme)
  "Return a completion annotation for THEME."
  (when-let* ((symbol (intern-soft theme))
              (doc-string (get symbol 'theme-documentation)))
    (format " -- %s"
            (propertize (car (split-string doc-string "\\."))
                        'face 'completions-annotations))))

(defun lackluster-theme--completion-table (category candidates)
  "Return completion metadata for CATEGORY and CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defvar lackluster-theme--select-theme-history nil
  "Minibuffer history for `lackluster-theme-select'.")

(defun lackluster-theme--select-prompt (&optional prompt)
  "Read a Lackluster theme from the minibuffer.

Use PROMPT if non-nil."
  (let* ((themes (lackluster-theme--completion-table
                  'theme
                  (lackluster-theme--enable-themes)))
         (completion-extra-properties
          `(:annotation-function ,#'lackluster-theme--annotate-theme)))
    (intern
     (completing-read (or prompt "Select Lackluster Theme: ")
                      themes nil t nil 'lackluster-theme--select-theme-history))))

(defun lackluster-theme--disable-themes ()
  "Disable all enabled themes before loading a Lackluster theme."
  (mapc #'disable-theme custom-enabled-themes))

(defun lackluster-theme-load-theme (theme)
  "Load THEME after disabling all other themes.

Run `lackluster-theme-post-load-hook' afterwards and return THEME."
  (lackluster-theme--disable-themes)
  (load-theme theme :no-confirm)
  (run-hooks 'lackluster-theme-post-load-hook)
  theme)

;;;###autoload
(defun lackluster-theme-select (theme)
  "Load a packaged Lackluster THEME using minibuffer completion."
  (interactive (list (lackluster-theme--select-prompt)))
  (lackluster-theme-load-theme theme))

(defun lackluster-theme--list-colors-get-mappings (palette)
  "Return semantic mappings from PALETTE.

String-valued entries are treated as raw colours and filtered out."
  (seq-remove
   (lambda (cell)
     (stringp (cadr cell)))
   palette))

(defun lackluster-theme--list-colors-tabulated (theme &optional mappings)
  "Return tabulated palette entries for THEME.

With MAPPINGS, only show semantic mappings."
  (let* ((current-palette (lackluster-theme--palette-value theme mappings))
         (palette (if mappings
                      (lackluster-theme--list-colors-get-mappings current-palette)
                    current-palette)))
    (mapcar
     (lambda (cell)
       (pcase-let* ((`(,name ,value) cell)
                    (name-string (format "%s" name))
                    (value-string (format "%s" value))
                    (value-string-padded (string-pad value-string 30))
                    (color (lackluster-theme--retrieve-palette-value name current-palette)))
         (list name
               (vector
                (if (and (symbolp value) (not (eq value 'unspecified))) "Yes" "")
                name-string
                (propertize value-string 'face `(:foreground ,color))
                (propertize value-string-padded
                            'face (list :background color
                                        :foreground
                                        (if (string= color "unspecified")
                                            (readable-foreground-color
                                             (lackluster-theme--retrieve-palette-value 'bg-main current-palette))
                                          (readable-foreground-color color))))))))
     palette)))

(defvar lackluster-theme-current-preview nil)
(defvar lackluster-theme-current-preview-show-mappings nil)

(defun lackluster-theme--set-tabulated-entries ()
  "Populate `tabulated-list-entries' for preview mode."
  (setq-local tabulated-list-entries
              (lackluster-theme--list-colors-tabulated
               lackluster-theme-current-preview
               lackluster-theme-current-preview-show-mappings)))

;;;###autoload
(defun lackluster-theme-list-colors (theme &optional mappings)
  "Preview palette entries for THEME.

With prefix argument MAPPINGS, show only semantic mappings."
  (interactive
   (let ((prompt (if current-prefix-arg
                     "Preview palette mappings of theme: "
                   "Preview palette of theme: ")))
     (list (lackluster-theme--select-prompt prompt)
           current-prefix-arg)))
  (let ((buffer (get-buffer-create
                 (format (if mappings "*%s-mappings*" "*%s-palette*") theme))))
    (with-current-buffer buffer
      (let ((lackluster-theme-current-preview theme)
            (lackluster-theme-current-preview-show-mappings mappings))
        (lackluster-theme-preview-mode)))
    (pop-to-buffer buffer)))

(define-derived-mode lackluster-theme-preview-mode tabulated-list-mode "Lackluster Palette"
  "Major mode used to preview Lackluster palette entries."
  :interactive nil
  (setq-local tabulated-list-format
              [("Mapping?" 10 t)
               ("Symbol name" 30 t)
               ("As foreground" 30 t)
               ("As background" 0 t)])
  (lackluster-theme--set-tabulated-entries)
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###theme-autoload
(deftheme lackluster
  "Strict, sparse baseline Lackluster variant."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'lackluster)

(eval-and-compile
  (defconst lackluster-palette
    '(
      ;; Base palette from lackluster.nvim
      (lack "#708090")
      (luster "#deeeed")
      (orange "#ffaa88")
      (yellow "#abab77")
      (green "#789978")
      (blue "#7788AA")
      (red "#D70000")
      (magenta lack)
      (black "#000000")

      (gray1 "#080808")
      (gray2 "#191919")
      (gray3 "#2a2a2a")
      (gray4 "#444444")
      (gray5 "#555555")
      (gray6 "#7a7a7a")
      (gray7 "#aaaaaa")
      (gray8 "#cccccc")
      (gray9 "#DDDDDD")

      ;; Core UI
      (bg-main "#101010")
      (fg-main gray8)
      (bg-dim gray2)
      (fg-dim gray5)
      (bg-alt "#1A1A1A")
      (fg-alt gray7)
      (bg-active gray3)
      (bg-inactive gray1)
      (border gray4)
      (cursor orange)
      (fg-intense gray9)
      (fg-search black)

      ;; UI states
      (bg-mode-line "#242424")
      (fg-mode-line gray7)
      (bg-completion gray3)
      (bg-hover gray2)
      (bg-hl-line gray2)
      (bg-region gray8)
      (fg-region black)
      (bg-err "#2b1816")
      (bg-warning "#2b2218")
      (bg-info "#182118")

      ;; Search and motion
      (bg-search-current lack)
      (bg-search-lazy gray3)
      (bg-search-replace "#31201d")
      (bg-search-match gray2)
      (bg-search-rx-group-0 "#182118")
      (bg-search-rx-group-1 "#2b1816")
      (bg-search-rx-group-2 "#2b2218")
      (bg-search-rx-group-3 "#1d2230")
      (bg-char-0 "#2b1816")
      (bg-char-1 "#182118")
      (bg-char-2 "#2b2218")
      (bg-paren gray3)
      (bg-red-intense "#7f2323")

      ;; Diffs
      (bg-added "#152015")
      (bg-added-faint "#111811")
      (bg-added-refine "#1d2b1d")
      (fg-added green)
      (bg-changed "#171714")
      (bg-changed-faint "#121211")
      (bg-changed-refine "#201d18")
      (fg-changed orange)
      (bg-removed "#211513")
      (bg-removed-faint "#1a1110")
      (bg-removed-refine "#2b1816")
      (fg-removed red)

      ;; Misc state colours
      (modeline-err red)
      (modeline-warning orange)
      (modeline-info lack)
      (underline-err red)
      (underline-warning orange)
      (underline-info lack)
      (link-alt lack)

      ;; Semantic mappings - Alabaster-like restraint with Lackluster colours
      (err red)
      (warning orange)
      (info green)
      (link blue)
      (name blue)
      (keybind orange)
      (identifier lack)
      (prompt blue)
      (builtin fg-main)
      (comment yellow)
      (constant green)
      (docstring comment)
      (fnname blue)
      (keyword fg-main)
      (preprocessor fg-main)
      (string green)
      (escape green)
      (type fg-main)
      (variable blue)
      (punctuation gray6)

      ;; Additional helpers
      (bg-fringe unspecified)
      (fg-fringe gray4)
      (fg-term-black "black")
      (fg-term-red red)
      (fg-term-green green)
      (fg-term-yellow orange)
      (fg-term-blue blue)
      (fg-term-magenta lack)
      (fg-term-cyan lack)
      (fg-term-white gray8)
      (bg-term-black "black")
      (bg-term-red red)
      (bg-term-green green)
      (bg-term-yellow orange)
      (bg-term-blue blue)
      (bg-term-magenta lack)
      (bg-term-cyan lack)
      (bg-term-white gray8)

      ;; Grayscale delimiter ramp
      (rainbow-0 gray5)
      (rainbow-1 gray6)
      (rainbow-2 gray7)
      (rainbow-3 gray8)
      (rainbow-4 gray4)
      (rainbow-5 gray5)
      (rainbow-6 gray6)
      (rainbow-7 gray7)
      (rainbow-8 gray8))
    "Palette for the baseline `lackluster' theme."))

(defcustom lackluster-palette-overrides nil
  "Overrides for `lackluster-palette'."
  :type '(repeat (list symbol (choice symbol string)))
  :group 'lackluster-theme)

(lackluster-theme--define lackluster lackluster-palette lackluster-palette-overrides)

(provide-theme 'lackluster)
(provide 'lackluster-theme)
;;; lackluster-theme.el ends here
