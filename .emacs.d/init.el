;;; init.el --- GNU Emacs initialization file.

;;; Commentary:

;; This is my init file.

;;; Code:

(require 'cl-lib)

;;;; Functions and macros

(defmacro substitute-var (var expr &rest body)
  "Substitute EXPR for VAR in BODY."
  (declare (indent 2))
  `(funcall (lambda (,var) ,@body) ,expr))

(defmacro dorange (plist &rest body)
  "Execute BODY with VAR ranging from START to END.
\(fn (VAR START END) BODY...)"
  (declare (indent 1))
  (cl-destructuring-bind (var start end) plist
    `(dotimes (,var (- ,end ,start))
       (substitute-var ,var (+ ,var ,start)
         ,@body))))

(defun column-number-of (pos)
  "Get the column number of POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun group-by (f xs)
  "Group the elements of XS into an alist using the projection F."
  (let ((table (ht-create)))
    (dolist (x xs)
      (let* ((y (funcall f x))
             (yxlist (ht-get table y)))
        (ht-set! table y (cons x yxlist))))
    (ht->alist table)))

(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buffer (buffer-list))
      (unless (eq buffer current)
        (kill-buffer buffer)))))

(defun repeats-from-to (str start end)
  "A list consisting of STR repeated n times, where n ranges from START to END - 1."
  (let ((acc (repeat-string start str))
        (ret))
    (dorange (n start end)
      (setq ret (cons acc ret))
    (setq acc (concat acc str)))
    (reverse ret)))

(defun repeat-string (n str)
  "Make a new string consisting of STR repeated N times."
  (apply 'concat (make-list n str)))

(defun underline-region (begin end chr)
  "Underline region (from BEGIN to END) with CHR."
  (interactive "r\ncUnderline character:")
  (let ((begin-col (column-number-of begin))
        (end-col (column-number-of end)))
    (save-excursion
      (move-end-of-line nil)
      (newline)
      (move-to-column begin-col t)
      (dotimes (_ (- end-col begin-col))
        (insert chr)))))

;;;; straight.el and use-package

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(if (version<= "24.5" emacs-version)
    ;; Use straight.el.
    (progn
      (setq straight-use-package-by-default t)
      (defvar bootstrap-version)
      (let ((bootstrap-file
             (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))
      (straight-use-package 'use-package)
      (setq straight t))
  ;; Fallback.
  (progn
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (setq straight nil)))

;;;; custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;; Utility packages

(use-package ht)

;;;; UI packages

(use-package doom-modeline
  :config
  (doom-modeline-mode t))
(use-package doom-themes)

(use-package monokai-theme)

(when straight
  (use-package spacemacs-theme))

;;;; UI

;; (load-theme 'doom-laserwave t)
(if (package-installed-p 'doom-themes)
    (load-theme 'doom-outrun-electric t)
  (load-theme 'monokai t))
;; (load-theme 'spacemacs-dark t)

(when window-system
  (let ((fonts '("Source Code Pro" "Fira Code")))
    (cl-dolist (font fonts)
      (when (x-list-fonts font)
	(set-face-attribute 'default nil
                            :family font
                            :height 140)
	(cl-return)))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-splash-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(setq column-number-mode t)
(when (version<= "26" emacs-version)
  (global-display-line-numbers-mode))

(setq show-paren-delay 0)
(show-paren-mode 1)

(global-hl-line-mode t)

(xterm-mouse-mode t)

;; Too often have I accidentally pressed this key combination.
;; The function is `save-buffers-kill-emacs`.
(global-unset-key (kbd "C-x C-c"))

;; Similarly...
(when window-system
  (global-unset-key (kbd "C-z")))

(let ((enable-firacode-ligatures nil))
  ;; TODO: FiraCode's ligature system is more powerful than this method can fully
  ;; accomodate.
  (when (and enable-firacode-ligatures
             (>= emacs-major-version 27)
             (equal (face-attribute 'default :family) "Fira Code"))
    (let* ((liga
            '("www" "--" "---" "-~" "{|" "|}" "]#" ".-" ".." "..." "..=" "..<"
              ".?" ".=" "::" ":::" "::=" ":=" ";;" "!!" "!!." "!=" "!==" "?." "??"
              "?=" "**" "***" "*>" "*/" "#(" "#{" "#[" "#:" "#!" "#?" "#=" "#_"
              "#_(" "/*" "/>" "//" "///" "/\\" "\\/" "&&" "|}" "|]" "||" "|||"
              "|||>" "||>" "|>" "$>" "++" "+++" "+>" "==" "===" ">=" ">>" ">>>"
              "<!--" "<*" "<*>" "<|" "<||" "<|||" "<|>" "<$" "<$>" "<+" "<+>" "<="
              "<>" "<<" "<<<" "<~" "<~>" "<~~" "</" "</>" "~-" "~@" "~>" "~~"
              "~~>" "^=" "%%"))
           (seq
            '(">>=" "=>" ">=>" "->" ">->" "=<<" "<=<" "<-" "<-<"))
           (salt
            '("Fl" "Il" "Tl" "fi" "fj"))
           (dash
            (repeats-from-to "-" 4 80))
           (misc
            '(";;;" ";;;;" "&&&" "^^=" "<-="))
           (all-ligatures
            (append liga seq salt dash misc))
           (ligature-alist
            (group-by (lambda (str) (aref str 0)) all-ligatures))
           (regexp-alist
            (mapcar (lambda (pair) (cons (car pair) (regexp-opt (cdr pair))))
                    ligature-alist)))
      (dolist (char-regexp regexp-alist)
        (set-char-table-range
         composition-function-table
         (car char-regexp)
         `([,(cdr char-regexp) 0 compose-gstring-for-graphic]))))))

;;;; Packages

(use-package attrap)

(use-package cargo)

(use-package counsel
  :init
  (counsel-mode))

(use-package company
  :init
  (setq company-idle-delay 0)
  (global-set-key (kbd "<C-tab>") 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package cuda-mode)

(use-package cython-mode)

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  ;; Just use v2-repl for everything.
  (setq dante-methods-alist
        `((v2-build
           ,(lambda (directory)
              (cl-some (apply-partially 'string-suffix-p ".cabal")
                       (directory-files directory)))
           ("cabal" "v2-repl")))
        dante-methods '(v2-build)))

(use-package dhall-mode)

(use-package elpy
  :demand
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package hasklig-mode
  :hook haskell-mode)

(use-package hideshow)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package idris-mode)

(use-package ivy
  :init
  (ivy-mode t))

(use-package lsp-haskell
  :config
  (add-hook
   'haskell-mode-hook
   (lambda ()
     (lsp)
     (lsp-lens-mode)))
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(use-package lsp-mode)

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t))

(use-package lua-mode)

(use-package macrostep)

(use-package magit)

(use-package opencl-mode)

(use-package pkgbuild-mode)

(use-package polymode
  :init
  (define-hostmode poly-sh-hostmode
    :mode 'sh-mode)
  (define-innermode poly-sh-awk-innermode
    :mode 'awk-mode
    :head-matcher "^[^#\n]*?awk[^#\n]*?'"
    :tail-matcher "'"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-sh-mode
    :hostmode 'poly-sh-hostmode
    :innermodes '(poly-sh-awk-innermode))
  (add-hook 'sh-mode-hook 'poly-sh-mode))

(use-package proof-general)

(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package purescript-mode
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package python-mode
  :init
  (add-hook
   'python-mode-hook
   (lambda ()
     (setq fill-column 72)))
  :config
  (setq python-fill-docstring-style 'django))

(use-package racer
  :init
  (add-hook
   'rust-mode-hook
   (lambda ()
     (racer-mode)
     (eldoc-mode)
     (company-mode)))
  (setq rust-format-on-save t))

(use-package racket-mode
  :init
  (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable))

(use-package rust-mode)

(use-package sunrise-commander)

(use-package tex
  :straight auctex
  :init
  (add-hook
   'TeX-mode-hook
   (lambda ()
     (visual-line-mode t)))
  :config
  (setq TeX-parse-self t
        TeX-auto-save t))

(use-package treemacs)

(use-package vterm)

(use-package writeroom-mode)

(use-package yaml-mode)

;;;; Backups

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;;; Org

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (lambda () (visual-line-mode t)))

;;;; Buffers

(setq initial-scratch-message nil)

;;;; Misc

(ignore-errors
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r"))
      c-basic-offset 4)

(setq erc-nick "nosewings")

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

;;;; Server

(server-start)

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
