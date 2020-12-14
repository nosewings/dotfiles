;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nicholas Coltharp"
      user-mail-address "coltharp@pdx.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font "Fira Code 12")
(setq doom-font "Fira Code")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-outrun-electric)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq auto-mode-case-fold t)

(use-package! agda-input
  :demand)

(use-package! page-break-lines
  :config
  (add-to-list 'page-break-lines-modes 'LaTeX-mode)
  (add-to-list 'page-break-lines-modes 'prog-mode)
  (add-to-list 'page-break-lines-modes 'text-mode)
  (global-page-break-lines-mode))

(after! latex
 (add-hook 'LaTeX-mode-hook (lambda ()
                               (add-to-list 'tex--prettify-symbols-alist '("\\lparen"     . #x28))
                               (add-to-list 'tex--prettify-symbols-alist '("\\rparen"     . #x29))
                               (add-to-list 'tex--prettify-symbols-alist '("\\lvert"      . #x7c))
                               (add-to-list 'tex--prettify-symbols-alist '("\\rvert"      . #x7c))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{I}"  . #x1d540))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{N}"  . #x2115))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{P}"  . #x2119))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{Q}"  . #x211a))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{R}"  . #x211d))
                               (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{Z}"  . #x2124))
                               (add-to-list 'tex--prettify-symbols-alist '("\\varnothing" . #x2205))
                               (add-to-list 'tex--prettify-symbols-alist '("\\land"       . #x2227))
                               (add-to-list 'tex--prettify-symbols-alist '("\\lor"        . #x2228))
                               (add-to-list 'tex--prettify-symbols-alist '("\\fcmp"       . #x2a3e))
                               (setq prettify-symbols-alist tex--prettify-symbols-alist))))

(use-package! lsp
  :custom
  ;; (lsp-lens-enable t)
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.live_mode" nil t))))

(define-derived-mode curry-mode haskell-mode "Curry"
  "Major mode for editing Curry programs."
  (font-lock-add-keywords nil '("free")))
(add-to-list 'auto-mode-alist '("\\.curry\\'" . curry-mode))

(define-derived-mode conf-dark-mode conf-mode "Conf[Dark]"
  (conf-mode-initialize ";"))
(add-to-list 'auto-mode-alist '("cam\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("cam_ext\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("dark\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("dromed\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("install\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("keybind\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("menus\\.cfg\\'" . conf-dark-mode))
(add-to-list 'auto-mode-alist '("user\\.cfg\\'" . conf-dark-mode))
